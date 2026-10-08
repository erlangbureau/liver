-module(liver_json_schema).

%% Bidirectional conversion between Liver field schemas and JSON Schema
%% (Draft 2020-12 dialect by default). Schema Objects overlap OpenAPI;
%% export reuses liver_openapi_schema rule mapping.

-export([to_json_schema/1, to_json_schema/2]).
-export([from_json_schema/1, from_json_schema/2]).

-define(DEFAULT_SCHEMA_URI,
    <<"https://json-schema.org/draft/2020-12/schema">>).

%%--------------------------------------------------------------------
%% Export: liver field schema / rule list → JSON Schema
%%--------------------------------------------------------------------

to_json_schema(SchemaOrRules) ->
    to_json_schema(SchemaOrRules, #{}).

to_json_schema(SchemaOrRules, Opts) when is_map(Opts) ->
    Raw0 = case SchemaOrRules of
        Schema when is_map(Schema) ->
            liver_openapi_schema:rules_to_openapi_schema([{nested_object, Schema}]);
        Rules when is_list(Rules) ->
            liver_openapi_schema:rules_to_openapi_schema(Rules)
    end,
    Raw1 = openapi_to_json_schema(Raw0),
    Raw = maybe_put_schema_uri(Raw1, Opts),
    case maps:get(output, Opts, raw) of
        raw ->
            Raw;
        json ->
            encode_json(Raw);
        file ->
            FileName = maps:get(filename, Opts, "schema.json"),
            file:write_file(FileName, encode_json(Raw))
    end.

%%--------------------------------------------------------------------
%% Import: JSON Schema → liver field schema
%%--------------------------------------------------------------------

from_json_schema(Schema) ->
    from_json_schema(Schema, #{}).

from_json_schema(Bin, Opts) when is_binary(Bin), is_map(Opts) ->
    from_json_schema(decode_json(Bin), Opts);
from_json_schema(Schema, Opts) when is_map(Schema), is_map(Opts) ->
    case maps:get(rule_set, Opts, erlang_standard) of
        livr_spec ->
            schema_to_field_schema(Schema, livr_spec);
        livr_compatible ->
            schema_to_field_schema(Schema, livr_spec);
        erlang_standard ->
            schema_to_field_schema(Schema, erlang_standard);
        Other ->
            {error, {unsupported, {rule_set, Other}}}
    end.

%%--------------------------------------------------------------------
%% JSON encode / decode (OTP 27+ json, else jsx)
%%--------------------------------------------------------------------

encode_json(Term) ->
    encode_json_impl(Term).

decode_json(Bin) when is_binary(Bin) ->
    decode_json_impl(Bin).

-if(?OTP_RELEASE >= 27).
encode_json_impl(Term) ->
    iolist_to_binary(json:encode(Term)).

decode_json_impl(Bin) ->
    json:decode(Bin).
-else.
encode_json_impl(Term) ->
    jsx:encode(Term).

decode_json_impl(Bin) ->
    jsx:decode(Bin, [return_maps]).
-endif.

%%--------------------------------------------------------------------
%% OpenAPI Schema Object → JSON Schema tweaks
%%--------------------------------------------------------------------

maybe_put_schema_uri(Schema, Opts) ->
    case maps:get(include_schema, Opts, true) of
        false ->
            Schema;
        true ->
            Uri = maps:get(schema_uri, Opts, ?DEFAULT_SCHEMA_URI),
            Schema#{<<"$schema">> => Uri}
    end.

%% Drop OpenAPI-only `nullable`; prefer type union with null when present.
openapi_to_json_schema(Schema) when is_map(Schema) ->
    Schema1 = maps:fold(
        fun(K, V, Acc) ->
            Acc#{K => openapi_to_json_schema(V)}
        end,
        #{},
        Schema
    ),
    case maps:take(nullable, Schema1) of
        {true, Rest} ->
            Type = maps:get(type, Rest, undefined),
            case Type of
                undefined -> Rest;
                T when is_list(T) ->
                    Rest#{type => lists:usort([null | T])};
                T ->
                    Rest#{type => [T, null]}
            end;
        {false, Rest} ->
            Rest;
        error ->
            case maps:take(<<"nullable">>, Schema1) of
                {true, Rest} ->
                    Type = schema_get(Rest, type),
                    case Type of
                        undefined -> Rest;
                        T when is_list(T) ->
                            Rest#{type => lists:usort([null | T])};
                        T ->
                            Rest#{type => [T, null]}
                    end;
                {false, Rest} ->
                    Rest;
                error ->
                    Schema1
            end
    end;
openapi_to_json_schema(List) when is_list(List) ->
    [openapi_to_json_schema(X) || X <- List];
openapi_to_json_schema(Other) ->
    Other.

%%--------------------------------------------------------------------
%% Import internals
%%--------------------------------------------------------------------

schema_to_field_schema(Schema, Dialect) ->
    case has_unsupported(Schema) of
        {true, Why} ->
            {error, {unsupported, Why}};
        false ->
            case normalize_type(schema_get(Schema, type)) of
                object -> object_to_field_schema(Schema, Dialect);
                undefined ->
                    case schema_get(Schema, properties) of
                        undefined -> {error, {unsupported, missing_type}};
                        _ -> object_to_field_schema(Schema, Dialect)
                    end;
                Other ->
                    {error, {unsupported, {top_level_type, Other}}}
            end
    end.

has_unsupported(Schema) ->
    case schema_get(Schema, <<"$ref">>) of
        undefined ->
            case schema_get(Schema, '$ref') of
                undefined ->
                    case schema_get(Schema, allOf) of
                        undefined ->
                            case schema_get(Schema, <<"$defs">>) of
                                undefined ->
                                    case schema_get(Schema, definitions) of
                                        undefined ->
                                            case schema_get(Schema, additionalProperties) of
                                                undefined -> false;
                                                false -> false;
                                                _ -> {true, additionalProperties}
                                            end;
                                        _ -> {true, definitions}
                                    end;
                                _ -> {true, defs}
                            end;
                        _ -> {true, allOf}
                    end;
                _ -> {true, ref}
            end;
        _ -> {true, ref}
    end.

object_to_field_schema(Schema, Dialect) ->
    case schema_get(Schema, oneOf) of
        undefined ->
            case schema_get(Schema, anyOf) of
                undefined ->
                    Props = case schema_get(Schema, properties) of
                        undefined -> #{};
                        P when is_map(P) -> P
                    end,
                    Required = case schema_get(Schema, required) of
                        undefined -> [];
                        R when is_list(R) -> R
                    end,
                    try
                        FieldSchema = maps:fold(
                            fun(K, PropSchema, Acc) ->
                                Key = ensure_key(K),
                                Rules0 = property_to_rules(PropSchema, Dialect),
                                Rules = case required_matches(Key, Required) of
                                    true -> [required | Rules0];
                                    false -> Rules0
                                end,
                                Acc#{Key => Rules}
                            end,
                            #{},
                            Props
                        ),
                        {ok, FieldSchema}
                    catch
                        throw:{error, _} = Err -> Err
                    end;
                _ ->
                    {error, {unsupported, anyOf}}
            end;
        _ ->
            {error, {unsupported, oneOf}}
    end.

required_matches(Key, Required) ->
    lists:any(
        fun(R) -> ensure_key(R) =:= Key orelse ensure_bin(R) =:= ensure_bin(Key) end,
        Required
    ).

property_to_rules(Schema, Dialect) when is_map(Schema) ->
    case has_unsupported(Schema) of
        {true, Why} -> throw({error, {unsupported, Why}});
        false -> ok
    end,
    case schema_get(Schema, oneOf) of
        undefined ->
            case schema_get(Schema, anyOf) of
                undefined ->
                    case schema_get(Schema, const) of
                        undefined -> type_to_rules(Schema, Dialect);
                        Const -> const_rules(Const, Dialect)
                    end;
                _ -> throw({error, {unsupported, anyOf}})
            end;
        _ ->
            throw({error, {unsupported, oneOf}})
    end.

const_rules(Const, erlang_standard) ->
    [{one_of_terms, [Const]}];
const_rules(Const, livr_spec) ->
    [{eq, Const}].

type_to_rules(Schema, Dialect) ->
    case normalize_type(schema_get(Schema, type)) of
        string -> string_rules(Schema, Dialect);
        integer -> integer_rules(Schema, Dialect);
        number -> number_rules(Schema, Dialect);
        boolean -> boolean_rules(Dialect);
        array -> array_rules(Schema, Dialect);
        object ->
            case object_to_field_schema(Schema, Dialect) of
                {ok, Nested} -> [nest_rule(Dialect, Nested)];
                {error, _} = Err -> throw(Err)
            end;
        null ->
            null_rules(Dialect);
        undefined ->
            case schema_get(Schema, properties) of
                undefined ->
                    case schema_get(Schema, enum) of
                        Enum when is_list(Enum), Enum =/= [] ->
                            enum_rules(Enum, Dialect);
                        _ ->
                            throw({error, {unsupported, missing_type}})
                    end;
                _ ->
                    case object_to_field_schema(Schema, Dialect) of
                        {ok, Nested} -> [nest_rule(Dialect, Nested)];
                        {error, _} = Err -> throw(Err)
                    end
            end;
        Other ->
            throw({error, {unsupported, {type, Other}}})
    end.

nest_rule(erlang_standard, Nested) -> {nested_map, Nested};
nest_rule(livr_spec, Nested) -> {nested_object, Nested}.

boolean_rules(erlang_standard) -> [is_boolean];
boolean_rules(livr_spec) -> [{one_of, [true, false]}].

null_rules(erlang_standard) -> [is_null];
null_rules(livr_spec) -> [{eq, null}].

enum_rules(Enum, erlang_standard) -> [{one_of_terms, Enum}];
enum_rules(Enum, livr_spec) -> [{one_of, Enum}].

string_rules(Schema, Dialect) ->
    Base = case schema_get(Schema, format) of
        <<"email">> -> [email];
        email -> [email];
        <<"uri">> -> [url];
        <<"url">> -> [url];
        uri -> [url];
        url -> [url];
        <<"date">> -> [iso_date];
        date -> [iso_date];
        _ -> default_string_rule(Dialect)
    end,
    PatternRules = case schema_get(Schema, pattern) of
        undefined -> [];
        Pattern when Dialect =:= livr_spec -> [{like, Pattern}];
        _ when Dialect =:= erlang_standard ->
            throw({error, {unsupported, pattern}})
    end,
    EnumRules = case schema_get(Schema, enum) of
        Enum when is_list(Enum), Enum =/= [] -> enum_rules(Enum, Dialect);
        _ -> []
    end,
    LenRules = string_length_rules(Schema, Dialect),
    Base ++ PatternRules ++ EnumRules ++ LenRules.

default_string_rule(erlang_standard) -> [is_utf8_binary];
default_string_rule(livr_spec) -> [string].

string_length_rules(Schema, erlang_standard) ->
    Min = schema_get(Schema, minLength),
    Max = schema_get(Schema, maxLength),
    case {Min, Max} of
        {undefined, undefined} -> [];
        {N, N} when is_integer(N) -> [{byte_size, [{eq, N}]}];
        {MinV, MaxV} when is_integer(MinV), is_integer(MaxV) ->
            [{byte_size, [{between, MinV, MaxV}]}];
        {MinV, undefined} when is_integer(MinV) -> [{byte_size, [{min, MinV}]}];
        {undefined, MaxV} when is_integer(MaxV) -> [{byte_size, [{max, MaxV}]}]
    end;
string_length_rules(Schema, livr_spec) ->
    Min = schema_get(Schema, minLength),
    Max = schema_get(Schema, maxLength),
    case {Min, Max} of
        {undefined, undefined} -> [];
        {N, N} when is_integer(N) -> [{length_equal, N}];
        {MinV, MaxV} when is_integer(MinV), is_integer(MaxV) ->
            [{length_between, [MinV, MaxV]}];
        {MinV, undefined} when is_integer(MinV) -> [{min_length, MinV}];
        {undefined, MaxV} when is_integer(MaxV) -> [{max_length, MaxV}]
    end.

integer_rules(Schema, erlang_standard) ->
    [is_integer | number_bound_rules(Schema, erlang_standard)];
integer_rules(Schema, livr_spec) ->
    [integer | number_bound_rules(Schema, livr_spec)].

number_rules(Schema, erlang_standard) ->
    [is_number | number_bound_rules(Schema, erlang_standard)];
number_rules(Schema, livr_spec) ->
    [decimal | number_bound_rules(Schema, livr_spec)].

number_bound_rules(Schema, erlang_standard) ->
    Min = schema_get(Schema, minimum),
    Max = schema_get(Schema, maximum),
    case {Min, Max} of
        {MinV, MaxV} when is_number(MinV), is_number(MaxV) ->
            [{range, [MinV, MaxV]}];
        _ ->
            []
    end;
number_bound_rules(Schema, livr_spec) ->
    Min = schema_get(Schema, minimum),
    Max = schema_get(Schema, maximum),
    case {Min, Max} of
        {MinV, MaxV} when is_number(MinV), is_number(MaxV) ->
            [{number_between, [MinV, MaxV]}];
        {MinV, undefined} when is_number(MinV) ->
            [{min_number, MinV}];
        {undefined, MaxV} when is_number(MaxV) ->
            [{max_number, MaxV}];
        _ ->
            []
    end.

array_rules(Schema, Dialect) ->
    Items = case schema_get(Schema, items) of
        undefined -> default_list_item(Dialect);
        ItemSchema when is_map(ItemSchema) -> property_to_rules(ItemSchema, Dialect)
    end,
    ItemRule = case Items of
        [One] -> One;
        Many when is_list(Many) -> Many
    end,
    [list_rule(Dialect, ItemRule)].

default_list_item(erlang_standard) -> [is_term];
default_list_item(livr_spec) -> [string].

list_rule(erlang_standard, ItemRule) -> {nested_list, ItemRule};
list_rule(livr_spec, ItemRule) -> {list_of, ItemRule}.

%% type may be atom, binary, or a list (union). Null-only unions strip null.
normalize_type(undefined) ->
    undefined;
normalize_type(Type) when is_list(Type) ->
    NonNull = [T || T <- Type, T =/= null, T =/= <<"null">>],
    case NonNull of
        [] -> null;
        [One] -> normalize_type(One);
        _ ->
            throw({error, {unsupported, {type_union, Type}}})
    end;
normalize_type(<<"string">>) -> string;
normalize_type(string) -> string;
normalize_type(<<"integer">>) -> integer;
normalize_type(integer) -> integer;
normalize_type(<<"number">>) -> number;
normalize_type(number) -> number;
normalize_type(<<"boolean">>) -> boolean;
normalize_type(boolean) -> boolean;
normalize_type(<<"array">>) -> array;
normalize_type(array) -> array;
normalize_type(<<"object">>) -> object;
normalize_type(object) -> object;
normalize_type(<<"null">>) -> null;
normalize_type(null) -> null;
normalize_type(Other) -> Other.

schema_get(Map, Key) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            Bin = atom_to_binary(Key, utf8),
            case maps:find(Bin, Map) of
                {ok, V} -> V;
                error -> undefined
            end
    end;
schema_get(Map, Key) when is_binary(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            try binary_to_existing_atom(Key, utf8) of
                Atom ->
                    case maps:find(Atom, Map) of
                        {ok, V} -> V;
                        error -> undefined
                    end
            catch
                error:badarg -> undefined
            end
    end.

ensure_key(K) when is_atom(K) -> K;
ensure_key(K) when is_binary(K) -> K;
ensure_key(K) when is_list(K) -> list_to_binary(K).

ensure_bin(B) when is_binary(B) -> B;
ensure_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_bin(L) when is_list(L) -> list_to_binary(L).
