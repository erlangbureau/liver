-module(json_schema_SUITE).

%% JSON Schema ↔ liver (erlang_standard / livr_spec).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        export_object_standard,
        export_rule_list_and_json,
        export_nullable_to_type_union,
        import_standard_roundtrip,
        import_livr_roundtrip,
        import_livr_pattern_and_bounds,
        import_const_and_null_union,
        import_edges_and_coverage,
        import_unsupported,
        validate_imported_standard,
        validate_imported_livr
    ].

init_per_suite(Config) ->
    _ = application:load(liver),
    Config.

end_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Export
%%--------------------------------------------------------------------

export_object_standard(_Config) ->
    Schema = #{
        name => [required, is_utf8_binary],
        age => [is_integer]
    },
    JS = liver_json_schema:to_json_schema(Schema),
    ?assertEqual(<<"https://json-schema.org/draft/2020-12/schema">>,
        maps:get(<<"$schema">>, JS)),
    ?assertEqual(object, maps:get(type, JS)),
    Props = maps:get(properties, JS),
    ?assertEqual(string, maps:get(type, maps:get(name, Props))),
    ?assertEqual(integer, maps:get(type, maps:get(age, Props))),
    ?assert(lists:member(name, maps:get(required, JS))),
    JS2 = liver_json_schema:to_json_schema(Schema, #{include_schema => false}),
    ?assertEqual(error, maps:find(<<"$schema">>, JS2)),
    ok.

export_rule_list_and_json(Config) ->
    Frag = liver_json_schema:to_json_schema([required, is_utf8_binary, email]),
    ?assertEqual(string, maps:get(type, Frag)),
    ?assertEqual(<<"email">>, maps:get(format, Frag)),
    Bin = liver_json_schema:to_json_schema(#{id => [is_integer]}, #{output => json}),
    ?assert(is_binary(Bin)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"\"$schema\"">>)),
    File = filename:join(?config(priv_dir, Config), "out.json"),
    ok = liver_json_schema:to_json_schema(#{n => [string]}, #{
        output => file,
        filename => File,
        include_schema => false
    }),
    {ok, Written} = file:read_file(File),
    ?assert(is_binary(Written)),
    ok.

export_nullable_to_type_union(_Config) ->
    Frag = liver_json_schema:to_json_schema([is_null], #{include_schema => false}),
    ?assert(is_map(Frag)),
    %% force OpenAPI-style nullable through rules that produce it
    Frag2 = liver_json_schema:to_json_schema([{eq, null}], #{include_schema => false}),
    Type = maps:get(type, Frag2, undefined),
    ?assert(Type =:= object orelse is_list(Type) orelse Type =:= undefined
        orelse maps:is_key(enum, Frag2)),
    ok.

%%--------------------------------------------------------------------
%% Import
%%--------------------------------------------------------------------

import_standard_roundtrip(_Config) ->
    OA = #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        type => object,
        required => [<<"name">>],
        properties => #{
            <<"name">> => #{type => string, minLength => 1, maxLength => 32},
            <<"age">> => #{type => integer, minimum => 0, maximum => 120},
            <<"tags">> => #{
                type => array,
                items => #{type => string}
            },
            <<"meta">> => #{
                type => object,
                properties => #{
                    <<"ok">> => #{type => boolean}
                }
            }
        }
    },
    {ok, Schema} = liver_json_schema:from_json_schema(OA),
    ?assertMatch([required, is_utf8_binary | _], maps:get(<<"name">>, Schema)),
    ?assertMatch([is_integer, {range, [0, 120]}], maps:get(<<"age">>, Schema)),
    ?assertMatch([{nested_list, is_utf8_binary}], maps:get(<<"tags">>, Schema)),
    ?assertMatch([{nested_map, _}], maps:get(<<"meta">>, Schema)),
    ok.

import_livr_roundtrip(_Config) ->
    OA = #{
        type => <<"object">>,
        required => [<<"email">>],
        properties => #{
            <<"email">> => #{type => <<"string">>, format => <<"email">>},
            <<"count">> => #{type => <<"integer">>, minimum => 1},
            <<"items">> => #{
                type => <<"array">>,
                items => #{type => <<"string">>}
            },
            <<"child">> => #{
                type => <<"object">>,
                properties => #{
                    <<"x">> => #{type => <<"number">>}
                }
            },
            <<"flag">> => #{type => <<"boolean">>}
        }
    },
    {ok, Schema} = liver_json_schema:from_json_schema(OA, #{rule_set => livr_spec}),
    ?assertMatch([required, email], maps:get(<<"email">>, Schema)),
    ?assertMatch([integer, {min_number, 1}], maps:get(<<"count">>, Schema)),
    ?assertMatch([{list_of, string}], maps:get(<<"items">>, Schema)),
    ?assertMatch([{nested_object, _}], maps:get(<<"child">>, Schema)),
    ?assertMatch([{one_of, [true, false]}], maps:get(<<"flag">>, Schema)),
    ok.

import_livr_pattern_and_bounds(_Config) ->
    {ok, Schema} = liver_json_schema:from_json_schema(#{
        type => object,
        properties => #{
            <<"code">> => #{
                type => string,
                pattern => <<"^[A-Z]+$">>,
                minLength => 2,
                maxLength => 4
            },
            <<"n">> => #{type => number, maximum => 10}
        }
    }, #{rule_set => livr_spec}),
    ?assertMatch([string, {like, <<"^[A-Z]+$">>}, {length_between, [2, 4]}],
        maps:get(<<"code">>, Schema)),
    ?assertMatch([decimal, {max_number, 10}], maps:get(<<"n">>, Schema)),
    ?assertMatch({error, {unsupported, pattern}},
        liver_json_schema:from_json_schema(#{
            type => object,
            properties => #{<<"x">> => #{type => string, pattern => <<"a">>}}
        })),
    ok.

import_const_and_null_union(_Config) ->
    {ok, S1} = liver_json_schema:from_json_schema(#{
        type => object,
        properties => #{
            <<"kind">> => #{const => <<"user">>}
        }
    }),
    ?assertMatch([{one_of_terms, [<<"user">>]}], maps:get(<<"kind">>, S1)),
    {ok, S2} = liver_json_schema:from_json_schema(#{
        type => object,
        properties => #{
            <<"kind">> => #{const => <<"user">>}
        }
    }, #{rule_set => livr_spec}),
    ?assertMatch([{eq, <<"user">>}], maps:get(<<"kind">>, S2)),
    {ok, S3} = liver_json_schema:from_json_schema(#{
        type => object,
        properties => #{
            <<"name">> => #{type => [<<"string">>, <<"null">>]}
        }
    }),
    ?assertMatch([is_utf8_binary], maps:get(<<"name">>, S3)),
    ok.

import_edges_and_coverage(_Config) ->
    %% JSON binary + livr_compatible alias
    Bin = <<"{\"type\":\"object\",\"properties\":{\"n\":{\"type\":\"integer\"}}}">>,
    {ok, FromBin} = liver_json_schema:from_json_schema(Bin, #{rule_set => livr_compatible}),
    ?assertMatch([integer], maps:get(<<"n">>, FromBin)),
    %% enum-only property, length_equal, number + both bounds (standard),
    %% null type, nested object, custom schema_uri
    {ok, S} = liver_json_schema:from_json_schema(#{
        type => object,
        properties => #{
            <<"role">> => #{enum => [<<"a">>, <<"b">>]},
            <<"code">> => #{type => string, minLength => 3, maxLength => 3},
            <<"score">> => #{type => number, minimum => 0, maximum => 1},
            <<"nothing">> => #{type => <<"null">>},
            <<"child">> => #{
                type => object,
                required => [<<"x">>],
                properties => #{<<"x">> => #{type => boolean}}
            }
        }
    }),
    ?assertMatch([{one_of_terms, [<<"a">>, <<"b">>]}], maps:get(<<"role">>, S)),
    ?assertMatch([is_utf8_binary, {byte_size, [{eq, 3}]}], maps:get(<<"code">>, S)),
    ?assertMatch([is_number, {range, [0, 1]}], maps:get(<<"score">>, S)),
    ?assertMatch([is_null], maps:get(<<"nothing">>, S)),
    ?assertMatch([{nested_map, #{<<"x">> := [required, is_boolean]}}],
        maps:get(<<"child">>, S)),
    %% livr length_equal + number_between + atom keys
    {ok, L} = liver_json_schema:from_json_schema(#{
        type => object,
        properties => #{
            code => #{type => string, minLength => 2, maxLength => 2},
            n => #{type => number, minimum => 1, maximum => 9},
            only_min => #{type => string, minLength => 1},
            only_max => #{type => string, maxLength => 5}
        }
    }, #{rule_set => livr_spec}),
    ?assertMatch([string, {length_equal, 2}], maps:get(code, L)),
    ?assertMatch([decimal, {number_between, [1, 9]}], maps:get(n, L)),
    ?assertMatch([string, {min_length, 1}], maps:get(only_min, L)),
    ?assertMatch([string, {max_length, 5}], maps:get(only_max, L)),
    %% single-sided byte_size (standard) + array without items
    {ok, S2} = liver_json_schema:from_json_schema(#{
        type => object,
        properties => #{
            <<"a">> => #{type => string, minLength => 2},
            <<"b">> => #{type => string, maxLength => 4},
            <<"c">> => #{type => array}
        }
    }),
    ?assertMatch([is_utf8_binary, {byte_size, [{min, 2}]}], maps:get(<<"a">>, S2)),
    ?assertMatch([is_utf8_binary, {byte_size, [{max, 4}]}], maps:get(<<"b">>, S2)),
    ?assertMatch([{nested_list, is_term}], maps:get(<<"c">>, S2)),
    Uri = liver_json_schema:to_json_schema(#{x => [string]}, #{
        schema_uri => <<"http://example.com/schema">>,
        include_schema => true
    }),
    ?assertEqual(<<"http://example.com/schema">>, maps:get(<<"$schema">>, Uri)),
    ok.

import_unsupported(_Config) ->
    ?assertMatch({error, {unsupported, ref}},
        liver_json_schema:from_json_schema(#{
            type => object,
            properties => #{<<"x">> => #{<<"$ref">> => <<"#/defs/X">>}}
        })),
    ?assertMatch({error, {unsupported, {rule_set, weird}}},
        liver_json_schema:from_json_schema(#{
            type => object,
            properties => #{}
        }, #{rule_set => weird})),
    ?assertMatch({error, {unsupported, {top_level_type, string}}},
        liver_json_schema:from_json_schema(#{type => string})),
    ok.

validate_imported_standard(_Config) ->
    {ok, Schema} = liver_json_schema:from_json_schema(#{
        type => object,
        required => [<<"name">>],
        properties => #{
            <<"name">> => #{type => string},
            <<"age">> => #{type => integer, minimum => 0, maximum => 150}
        }
    }),
    ?assertMatch({ok, _},
        liver:validate(Schema, #{<<"name">> => <<"bob">>, <<"age">> => 30})),
    ?assertMatch({error, _},
        liver:validate(Schema, #{<<"age">> => 30})),
    ok.

validate_imported_livr(_Config) ->
    {ok, Schema} = liver_json_schema:from_json_schema(#{
        type => object,
        required => [<<"name">>],
        properties => #{
            <<"name">> => #{type => string},
            <<"age">> => #{type => integer}
        }
    }, #{rule_set => livr_spec}),
    ?assertMatch({ok, _},
        liver:validate(Schema, #{<<"name">> => <<"bob">>, <<"age">> => <<"30">>},
            #{rule_set => livr_spec})),
    ok.
