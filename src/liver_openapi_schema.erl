-module(liver_openapi_schema).

%% OpenAPI helpers:
%%   export — liver validation schema → OpenAPI 3 document / Schema Object
%%   import — OpenAPI Schema Object → erlang_standard liver schema

-export([generate/1, generate/2]).
-export([generate_schema/1, generate_schema/2]).
-export([rules_to_openapi_schema/1]).
-export([from_openapi_schema/1, from_openapi_schema/2]).

-define(DEFAULT_RESPONSE_SCHEMA,
    [{variable_object, [<<"status">>, [
        {<<"ok">>, #{
            <<"response">> => [any_object]
        }},
        {<<"error">>, #{
            <<"code">> => [required, integer],
            <<"message">> => [required, string]
        }}]
    ]}]
).

%%--------------------------------------------------------------------
%% Export: module callback or path map → OpenAPI document
%%--------------------------------------------------------------------

generate(ValidationModule) ->
    generate(ValidationModule, #{}).

generate(ValidationModule, Opts) when is_atom(ValidationModule), is_map(Opts) ->
    generate_schema(ValidationModule:liver_schema(), Opts).

generate_schema(PathSchema) ->
    generate_schema(PathSchema, #{}).

generate_schema(PathSchema, Opts) when is_map(PathSchema), is_map(Opts) ->
    Raw = document_schema(PathSchema, Opts),
    case maps:get(output, Opts, json) of
        raw ->
            Raw;
        json ->
            encode_json(Raw);
        file ->
            FileName = maps:get(filename, Opts, "openapi.json"),
            file:write_file(FileName, encode_json(Raw))
    end.

%%--------------------------------------------------------------------
%% Import: OpenAPI Schema Object → erlang_standard field schema
%%--------------------------------------------------------------------

from_openapi_schema(Schema) ->
    from_openapi_schema(Schema, #{}).

from_openapi_schema(Bin, Opts) when is_binary(Bin), is_map(Opts) ->
    from_openapi_schema(decode_json(Bin), Opts);
from_openapi_schema(Schema, Opts) when is_map(Schema), is_map(Opts) ->
    case schema_to_field_schema(Schema) of
        {ok, FieldSchema} ->
            {ok, FieldSchema};
        {error, _} = Err ->
            Err
    end.

%%--------------------------------------------------------------------
%% Export internals
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

document_schema(PathSchema, Opts) ->
    Host = maps:get(host, Opts, <<"127.0.0.1">>),
    Port = maps:get(port, Opts, <<"80">>),
    Info = maps:get(info, Opts, #{
        title => <<"API">>,
        version => <<"0.1.0">>,
        description => <<>>
    }),
    ResponseSchema = maps:get(response_schema, Opts, ?DEFAULT_RESPONSE_SCHEMA),
    Meta = maps:get(meta, Opts, #{}),
    Paths = maps:fold(
        fun(Path, ReqSchema, Acc) ->
            Acc#{ensure_bin(Path) => #{
                <<"post">> => method_schema(ReqSchema, ResponseSchema, Meta)
            }}
        end,
        #{},
        PathSchema
    ),
    #{
        openapi => <<"3.0.3">>,
        info => Info,
        servers => [#{
            url => <<"https://{host}:{port}/">>,
            variables => #{
                host => #{default => Host},
                port => #{default => Port}
            }
        }],
        paths => Paths
    }.

method_schema(RequestSchema, ResponseSchema, Meta) ->
    #{
        tags => maps:get(tags, Meta, [<<"rpc">>]),
        description => maps:get(doc, Meta, <<>>),
        requestBody => #{
            required => true,
            content => #{
                <<"application/json">> => #{
                    schema => rules_to_openapi_schema([{nested_object, RequestSchema}])
                }
            }
        },
        responses => #{
            <<"200">> => #{
                description => maps:get(response_description, Meta, <<>>),
                content => #{
                    <<"application/json">> => #{
                        schema => rules_to_openapi_schema(ResponseSchema)
                    }
                }
            }
        }
    }.

rules_to_openapi_schema(Rules) ->
    NormalizedRules = liver_rules:normalize(Rules, #{}),
    lists:foldl(
        fun(Rule, Acc) -> maps:merge(Acc, to_openapi_schema(Rule)) end,
        #{},
        NormalizedRules
    ).

%% common / LIVR
to_openapi_schema({required, _}) ->
    #{};
to_openapi_schema({not_empty, _}) ->
    #{'not' => #{enum => [<<>>]}};
to_openapi_schema({not_empty_list, _}) ->
    #{type => array, minItems => 1, items => #{type => object}};
to_openapi_schema({any_object, _}) ->
    #{type => object};
%% string (LIVR)
to_openapi_schema({string, _}) ->
    #{type => string};
to_openapi_schema({eq, [Value]}) ->
    to_openapi_schema({eq, Value});
to_openapi_schema({eq, null}) ->
    #{type => object, nullable => true};
to_openapi_schema({eq, Value}) ->
    #{type => guess_type(Value), example => Value, enum => [Value]};
to_openapi_schema({one_of, [FirstValue | _] = Values}) ->
    #{type => guess_type(FirstValue), enum => Values};
to_openapi_schema({max_length, Length}) ->
    #{type => string, maxLength => Length};
to_openapi_schema({min_length, Length}) ->
    #{type => string, minLength => Length};
to_openapi_schema({length_between, [MinLength, MaxLength]}) ->
    #{type => string, minLength => MinLength, maxLength => MaxLength};
to_openapi_schema({length_equal, Length}) ->
    #{type => string, minLength => Length, maxLength => Length};
to_openapi_schema({like, Pattern}) ->
    #{type => string, pattern => Pattern};
%% numeric (LIVR)
to_openapi_schema({integer, _}) ->
    #{type => integer};
to_openapi_schema({positive_integer, _}) ->
    #{type => integer, minimum => 1};
to_openapi_schema({decimal, _}) ->
    #{type => number};
to_openapi_schema({positive_decimal, _}) ->
    #{type => number, minimum => 0, exclusiveMinimum => true};
to_openapi_schema({max_number, MaxNumber}) ->
    #{type => number, maximum => MaxNumber};
to_openapi_schema({min_number, MinNumber}) ->
    #{type => number, minimum => MinNumber};
to_openapi_schema({number_between, [MinNumber, MaxNumber]}) ->
    #{type => number, minimum => MinNumber, maximum => MaxNumber};
%% special
to_openapi_schema({email, _}) ->
    #{type => string, format => <<"email">>};
to_openapi_schema({url, _}) ->
    #{type => string, format => <<"uri">>};
to_openapi_schema({iso_date, _}) ->
    #{type => string, format => <<"date">>};
to_openapi_schema({equal_to_field, _}) ->
    #{type => object};
%% meta (LIVR + standard aliases)
to_openapi_schema({nested_object, Schema}) ->
    object_schema(Schema);
to_openapi_schema({nested_map, Schema}) ->
    object_schema(Schema);
to_openapi_schema({nested_proplist, Schema}) ->
    object_schema(Schema);
to_openapi_schema({variable_object, [VariableKey, MapsSchema]}) when is_map(MapsSchema) ->
    to_openapi_schema({variable_object, [VariableKey, maps:to_list(MapsSchema)]});
to_openapi_schema({variable_object, [VariableKey, ListSchema]}) when is_list(ListSchema) ->
    Schemas = lists:map(
        fun
            ({VariableValue, SubSchema}) when is_map(SubSchema) ->
                to_openapi_schema({nested_object,
                    SubSchema#{VariableKey => [required, {eq, VariableValue}]}});
            ({VariableValue, SubSchema}) when is_list(SubSchema) ->
                to_openapi_schema({nested_object,
                    [{VariableKey, [required, {eq, VariableValue}]} | SubSchema]})
        end,
        ListSchema
    ),
    #{oneOf => Schemas};
to_openapi_schema({list_of, Rules}) ->
    #{type => array, items => rules_to_openapi_schema(Rules)};
to_openapi_schema({nested_list, Rules}) ->
    #{type => array, items => rules_to_openapi_schema(Rules)};
to_openapi_schema({list_of_objects, [Rules | _]}) when is_list(Rules); is_map(Rules) ->
    to_openapi_schema({list_of_objects, Rules});
to_openapi_schema({list_of_objects, Rules}) ->
    #{type => array, items => to_openapi_schema({nested_object, Rules})};
to_openapi_schema({list_of_different_objects, [Rules | _]}) when is_list(Rules) ->
    to_openapi_schema({list_of_different_objects, Rules});
to_openapi_schema({list_of_different_objects, Rules}) ->
    #{type => array, items => to_openapi_schema({variable_object, Rules})};
to_openapi_schema({'or', Rules}) ->
    Schemas = lists:map(
        fun(InternalRules) ->
            rules_to_openapi_schema(liver_rules:normalize(InternalRules, #{}))
        end,
        Rules
    ),
    #{oneOf => Schemas};
%% erlang_standard type predicates
to_openapi_schema({is_integer, _}) ->
    #{type => integer};
to_openapi_schema({is_non_neg_integer, _}) ->
    #{type => integer, minimum => 0};
to_openapi_schema({is_pos_integer, _}) ->
    #{type => integer, minimum => 1};
to_openapi_schema({is_float, _}) ->
    #{type => number};
to_openapi_schema({is_number, _}) ->
    #{type => number};
to_openapi_schema({is_boolean, _}) ->
    #{type => boolean};
to_openapi_schema({is_utf8_binary, _}) ->
    #{type => string};
to_openapi_schema({is_binary, _}) ->
    #{type => string, format => <<"byte">>};
to_openapi_schema({is_string, _}) ->
    #{type => string};
to_openapi_schema({is_atom, _}) ->
    #{type => string};
to_openapi_schema({is_list, _}) ->
    #{type => array, items => #{}};
to_openapi_schema({is_map, _}) ->
    #{type => object};
to_openapi_schema({is_proplist, _}) ->
    #{type => object};
to_openapi_schema({is_null, _}) ->
    #{nullable => true, enum => [null]};
to_openapi_schema({is_not_null, _}) ->
    #{};
to_openapi_schema({is_undefined, _}) ->
    #{};
to_openapi_schema({is_not_undefined, _}) ->
    #{};
to_openapi_schema({is_term, _}) ->
    #{};
to_openapi_schema({is_tuple, _}) ->
    #{type => array};
to_openapi_schema({is_pid, _}) ->
    #{};
to_openapi_schema({is_ref, _}) ->
    #{};
to_openapi_schema({is_port, _}) ->
    #{};
to_openapi_schema({is_fun, _}) ->
    #{};
%% erlang_standard constraints
to_openapi_schema({one_of_terms, [Allowed]}) when is_list(Allowed) ->
    to_openapi_schema({one_of_terms, Allowed});
to_openapi_schema({one_of_terms, Allowed}) when is_list(Allowed), Allowed =/= [] ->
    #{type => guess_type(hd(Allowed)), enum => Allowed};
to_openapi_schema({member, Args}) ->
    to_openapi_schema({one_of_terms, Args});
to_openapi_schema({range, [{Min, Max}]}) ->
    to_openapi_schema({range, [Min, Max]});
to_openapi_schema({range, [Min, Max]}) ->
    #{type => number, minimum => Min, maximum => Max};
to_openapi_schema({byte_size, [{eq, N}]}) ->
    #{type => string, minLength => N, maxLength => N};
to_openapi_schema({byte_size, [{min, Min}]}) ->
    #{type => string, minLength => Min};
to_openapi_schema({byte_size, [{max, Max}]}) ->
    #{type => string, maxLength => Max};
to_openapi_schema({byte_size, [{between, Min, Max}]}) ->
    #{type => string, minLength => Min, maxLength => Max};
to_openapi_schema({byte_size, _}) ->
    #{type => string};
to_openapi_schema({length, [{eq, N}]}) ->
    #{type => array, minItems => N, maxItems => N};
to_openapi_schema({length, [{min, Min}]}) ->
    #{type => array, minItems => Min};
to_openapi_schema({length, [{max, Max}]}) ->
    #{type => array, maxItems => Max};
to_openapi_schema({length, _}) ->
    #{type => array};
%% converters / modifiers — no schema fragment
to_openapi_schema({Rule, _})
  when Rule =:= trim;
       Rule =:= to_lc;
       Rule =:= to_uc;
       Rule =:= remove;
       Rule =:= leave_only;
       Rule =:= default;
       Rule =:= to_integer;
       Rule =:= to_float;
       Rule =:= to_boolean;
       Rule =:= to_string;
       Rule =:= to_utf8_binary;
       Rule =:= to_binary;
       Rule =:= to_atom;
       Rule =:= to_existing_atom;
       Rule =:= to_list;
       Rule =:= to_map;
       Rule =:= to_proplist;
       Rule =:= bit_size;
       Rule =:= tuple_size;
       Rule =:= map_size ->
    #{};
to_openapi_schema({UnknownRule, _}) ->
    throw({error, {unknown_rule, UnknownRule}}).

object_schema(ListSchema) when is_list(ListSchema) ->
    object_schema(maps:from_list(ListSchema));
object_schema(MapsSchema) when is_map(MapsSchema) ->
    Init = case maps_required(MapsSchema) of
        [] -> #{};
        Required -> #{required => Required}
    end,
    Properties = maps:map(
        fun(_K, Rules) -> rules_to_openapi_schema(Rules) end,
        MapsSchema
    ),
    Init#{type => object, properties => Properties}.

maps_required(Schema) ->
    maps:fold(
        fun(K, Rules, Acc) ->
            Normalized = liver_rules:normalize(Rules, #{}),
            case lists:keymember(required, 1, Normalized) of
                true -> [K | Acc];
                false -> Acc
            end
        end,
        [],
        Schema
    ).

guess_type(V) when is_boolean(V) -> boolean;
guess_type(V) when is_list(V) -> array;
guess_type(V) when is_integer(V) -> integer;
guess_type(V) when is_float(V) -> number;
guess_type(V) when is_binary(V) -> string;
guess_type(V) when is_atom(V) -> string;
guess_type(_) -> object.

ensure_bin(B) when is_binary(B) -> B;
ensure_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_bin(L) when is_list(L) -> list_to_binary(L).

%%--------------------------------------------------------------------
%% Import internals → erlang_standard
%%--------------------------------------------------------------------

schema_to_field_schema(Schema) ->
    case has_unsupported(Schema) of
        {true, Why} ->
            {error, {unsupported, Why}};
        false ->
            case schema_get(Schema, type) of
                <<"object">> -> object_to_field_schema(Schema);
                object -> object_to_field_schema(Schema);
                undefined ->
                    %% properties without type still treated as object
                    case schema_get(Schema, properties) of
                        undefined -> {error, {unsupported, missing_type}};
                        _ -> object_to_field_schema(Schema)
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
                            case schema_get(Schema, additionalProperties) of
                                undefined -> false;
                                _ -> {true, additionalProperties}
                            end;
                        _ -> {true, allOf}
                    end;
                _ -> {true, ref}
            end;
        _ -> {true, ref}
    end.

object_to_field_schema(Schema) ->
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
                                Rules0 = property_to_rules(PropSchema),
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

property_to_rules(Schema) when is_map(Schema) ->
    case has_unsupported(Schema) of
        {true, Why} -> throw({error, {unsupported, Why}});
        false -> ok
    end,
    case schema_get(Schema, oneOf) of
        undefined ->
            case schema_get(Schema, anyOf) of
                undefined -> type_to_rules(Schema);
                _ -> throw({error, {unsupported, anyOf}})
            end;
        _ ->
            throw({error, {unsupported, oneOf}})
    end.

type_to_rules(Schema) ->
    Type = schema_get(Schema, type),
    %% nullable is documented limitation: ignored; null will not pass type checks
    case Type of
        <<"string">> -> string_rules(Schema);
        string -> string_rules(Schema);
        <<"integer">> -> integer_rules(Schema);
        integer -> integer_rules(Schema);
        <<"number">> -> number_rules(Schema);
        number -> number_rules(Schema);
        <<"boolean">> -> [is_boolean];
        boolean -> [is_boolean];
        <<"array">> -> array_rules(Schema);
        array -> array_rules(Schema);
        <<"object">> ->
            case object_to_field_schema(Schema) of
                {ok, Nested} -> [{nested_map, Nested}];
                {error, _} = Err -> throw(Err)
            end;
        object ->
            case object_to_field_schema(Schema) of
                {ok, Nested} -> [{nested_map, Nested}];
                {error, _} = Err -> throw(Err)
            end;
        undefined ->
            case schema_get(Schema, properties) of
                undefined ->
                    case schema_get(Schema, enum) of
                        Enum when is_list(Enum), Enum =/= [] ->
                            [{one_of_terms, Enum}];
                        _ ->
                            throw({error, {unsupported, missing_type}})
                    end;
                _ ->
                    case object_to_field_schema(Schema) of
                        {ok, Nested} -> [{nested_map, Nested}];
                        {error, _} = Err -> throw(Err)
                    end
            end;
        Other ->
            throw({error, {unsupported, {type, Other}}})
    end.

string_rules(Schema) ->
    case schema_get(Schema, pattern) of
        undefined -> ok;
        _ -> throw({error, {unsupported, pattern}})
    end,
    Base = case schema_get(Schema, format) of
        <<"email">> -> [email];
        email -> [email];
        <<"uri">> -> [url];
        <<"url">> -> [url];
        uri -> [url];
        url -> [url];
        <<"date">> -> [iso_date];
        date -> [iso_date];
        _ -> [is_utf8_binary]
    end,
    EnumRules = case schema_get(Schema, enum) of
        Enum when is_list(Enum), Enum =/= [] -> [{one_of_terms, Enum}];
        _ -> []
    end,
    LenRules = string_length_rules(Schema),
    Base ++ EnumRules ++ LenRules.

string_length_rules(Schema) ->
    Min = schema_get(Schema, minLength),
    Max = schema_get(Schema, maxLength),
    case {Min, Max} of
        {undefined, undefined} -> [];
        {N, N} when is_integer(N) -> [{byte_size, [{eq, N}]}];
        {MinV, MaxV} when is_integer(MinV), is_integer(MaxV) ->
            [{byte_size, [{between, MinV, MaxV}]}];
        {MinV, undefined} when is_integer(MinV) -> [{byte_size, [{min, MinV}]}];
        {undefined, MaxV} when is_integer(MaxV) -> [{byte_size, [{max, MaxV}]}]
    end.

integer_rules(Schema) ->
    [is_integer | number_bound_rules(Schema)].

number_rules(Schema) ->
    [is_number | number_bound_rules(Schema)].

number_bound_rules(Schema) ->
    Min = schema_get(Schema, minimum),
    Max = schema_get(Schema, maximum),
    case {Min, Max} of
        {MinV, MaxV} when is_number(MinV), is_number(MaxV) ->
            [{range, [MinV, MaxV]}];
        _ ->
            %% Single-sided bounds need a dedicated rule; skipped in MVP.
            []
    end.

array_rules(Schema) ->
    Items = case schema_get(Schema, items) of
        undefined -> [is_term];
        ItemSchema when is_map(ItemSchema) -> property_to_rules(ItemSchema)
    end,
    ItemRule = case Items of
        [One] -> One;
        Many when is_list(Many) -> Many
    end,
    [{nested_list, ItemRule}].

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
