-module(liver_openapi_schema).

%% API
-export([generate/1, generate/2]).

-define(ResponseSchema,
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


%% API
generate(ValidationModule) ->
    generate(ValidationModule, #{}).

generate(ValidationModule, Opts) ->
    RawSchema = document_schema(ValidationModule, Opts),
    case maps:get(output, Opts, json) of
        raw ->
            RawSchema;
        json ->
            jsx:encode(RawSchema);
        file ->
            FileName = maps:get(filename, Opts, "swagger.json"),
            Schema = jsx:encode(RawSchema),
            file:write_file(FileName, Schema)
    end.

%% internal
document_schema(ValidationModule, Opts) ->
    Host = maps:get(host, Opts, <<"127.0.0.1">>),
    Port = maps:get(port, Opts, <<"80">>),
    LiverSchema = ValidationModule:liver_schema(),
    Fun = fun(Path, ReqSchema, PathsAcc) ->
            PathsAcc#{Path => #{<<"post">> => method_schema(ReqSchema, ?ResponseSchema, #{})}}
    end,
    Paths = maps:fold(Fun, #{}, LiverSchema),
    #{
        openapi => <<"3.0.3">>,
        info => #{title => <<"API">>, version => <<"0.1.0">>, description => <<>>},
        servers => [#{
            url => <<"https://{host}:{port}/">>,
            variables => #{
                host => #{default => Host},
                port => #{default => Port}
            }
        }],
        paths => lists:sort(maps:to_list(Paths))
    }.

method_schema(RequestSchema, ResponseSchema, Meta) ->
    #{
        tags => maps:get(tags, Meta, [<<"rpc">>]),
        description => maps:get(doc, Meta, <<>>),
        requestBody => #{required => true, content => #{<<"application/json">> => #{
            schema => rules_to_openapi_schema([{nested_object, RequestSchema}])
        }}},
        responses => #{
            <<"200">> => #{
                description => <<>>, %% TODO add response description
                content => #{<<"application/json">> => #{
                    schema => rules_to_openapi_schema(ResponseSchema)
                }}
            }
        }
    }.

rules_to_openapi_schema(Rules) ->
    NormalizedRules = liver_rules:normalize(Rules, #{}),
    Fun = fun(Rule, Acc) ->
        maps:merge(Acc, to_openapi_schema(Rule))
    end,
    lists:foldl(Fun, #{}, NormalizedRules).

%% common
to_openapi_schema({required, _}) -> %% processed on object level
    #{};
to_openapi_schema({not_empty, _}) ->
    #{'not' => #{enum => [<<>>]}};
to_openapi_schema({not_empty_list, _}) ->
    #{type => array, minItems => 1, items => #{type => object}};
to_openapi_schema({any_object, _}) ->
    #{type => object};
%% string
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
%% numeric
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
    #{type => string, format => email};
to_openapi_schema({url, _}) ->
    #{type => string, format => url};
to_openapi_schema({iso_date, _}) ->
    #{type => string, format => date};
to_openapi_schema({equal_to_field, _}) -> %% TODO is it possible to describe this in openapi?
    #{type => object};
%% meta rules
to_openapi_schema({nested_object, ListSchema}) when is_list(ListSchema) ->
    to_openapi_schema({nested_object, maps:from_list(ListSchema)});
to_openapi_schema({nested_object, MapsSchema}) when is_map(MapsSchema) ->
    InitSchema = case maps_required(MapsSchema) of
        [] -> #{};
        Required -> #{required => Required}
    end,
    Fun = fun(K, Rules, SchemaAcc) ->
        [{K, rules_to_openapi_schema(Rules)} | SchemaAcc]
    end,
    Properties = maps:fold(Fun, [], MapsSchema),
    InitSchema#{type => object, properties => lists:sort(Properties)};
to_openapi_schema({variable_object, [VariableKey, MapsSchema]}) when is_map(MapsSchema) ->
    to_openapi_schema({variable_object, [VariableKey, maps:to_list(MapsSchema)]});
to_openapi_schema({variable_object, [VariableKey, ListSchema]}) when is_list(ListSchema) ->
    Schemas = lists:map(fun
        ({VariableValue, SubSchema}) when is_map(SubSchema) ->
            to_openapi_schema({nested_object, SubSchema#{VariableKey => [required, {eq, VariableValue}]}});
        ({VariableValue, SubSchema}) when is_list(SubSchema) ->
            to_openapi_schema({nested_object, [{VariableKey, [required, {eq, VariableValue}]} | SubSchema]})
    end, ListSchema),
    #{oneOf => Schemas};
to_openapi_schema({list_of, Rules}) ->
    #{type => array, items => rules_to_openapi_schema(Rules)};
to_openapi_schema({list_of_objects, [Rules|_]}) when is_list(Rules); is_map(Rules) ->
    to_openapi_schema({list_of_objects, Rules});
to_openapi_schema({list_of_objects, Rules}) ->
    #{type => array, items => to_openapi_schema({nested_object, Rules})};
to_openapi_schema({list_of_different_objects, [Rules|_]}) when is_list(Rules) ->
    to_openapi_schema({list_of_different_objects, Rules});
to_openapi_schema({list_of_different_objects, Rules}) ->
    #{type => array, items => to_openapi_schema({variable_object, Rules})};
to_openapi_schema({'or', Rules}) ->
    Fun = fun(InternalRules) ->
        NormalizedRules = liver_rules:normalize(InternalRules, #{}),
        rules_to_openapi_schema(NormalizedRules)
    end,
    Schemas = lists:map(Fun, Rules),
    #{oneOf => Schemas};
%% modifiers, they modify validation args, no need in description
to_openapi_schema({Rule, _})
    when
        Rule == trim;
        Rule == to_lc;
        Rule == to_uc;
        Rule == remove;
        Rule == leave_only;
        Rule == default ->
    #{};
to_openapi_schema({UnknownRule, _}) ->
    throw({error, {unknown_rule, UnknownRule}}).

maps_required(Schema) ->
    Fun = fun(K, Rules, RequiredAcc) ->
        NormalizedRules = liver_rules:normalize(Rules, #{}),
        case lists:keymember(required, 1, NormalizedRules) of
            true -> [K | RequiredAcc];
            false -> RequiredAcc
        end
    end,
    maps:fold(Fun, [], Schema).

guess_type(V) when is_boolean(V) ->
    boolean;
guess_type(V) when is_list(V) ->
    array;
guess_type(V) when is_integer(V) ->
    integer;
guess_type(V) when is_float(V) ->
    number;
guess_type(V) when is_binary(V) ->
    string;
guess_type(_) ->
    #{type => object}.
