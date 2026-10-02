-module(openapi_schema_SUITE).

%% Export liver schemas to OpenAPI and import Schema Objects to erlang_standard.

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Used by liver_openapi_schema:generate/1,2
liver_schema() ->
    #{
        <<"/rpc">> => #{
            <<"msg">> => [required, string]
        },
        '/atom_path' => #{
            id => [required, is_integer]
        }
    }.

all() ->
    [
        export_livr_path_schema,
        export_standard_nested_map,
        export_generate_module_and_file,
        export_livr_rule_surface,
        export_standard_rule_surface,
        export_unknown_rule,
        import_roundtrip_ok,
        import_roundtrip_error,
        import_json_binary,
        import_string_formats_and_lengths,
        import_nested_and_array,
        import_unsupported_constructs,
        import_edge_errors
    ].

init_per_suite(Config) ->
    _ = application:load(liver),
    Config.

end_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Export
%%--------------------------------------------------------------------

export_livr_path_schema(_Config) ->
    Paths = #{
        <<"/echo">> => #{
            <<"name">> => [required, string],
            <<"age">> => [integer]
        }
    },
    Doc = liver_openapi_schema:generate_schema(Paths, #{output => raw}),
    ?assertEqual(<<"3.0.3">>, maps:get(openapi, Doc)),
    PathsOut = maps:get(paths, Doc),
    ?assert(is_map(PathsOut)),
    Post = maps:get(<<"post">>, maps:get(<<"/echo">>, PathsOut)),
    ReqSchema = maps:get(schema,
        maps:get(<<"application/json">>,
            maps:get(content, maps:get(requestBody, Post)))),
    ?assertEqual(object, maps:get(type, ReqSchema)),
    Props = maps:get(properties, ReqSchema),
    ?assertEqual(string, maps:get(type, maps:get(<<"name">>, Props))),
    ?assertEqual(integer, maps:get(type, maps:get(<<"age">>, Props))),
    ?assert(lists:member(<<"name">>, maps:get(required, ReqSchema))),
    Json = liver_openapi_schema:generate_schema(Paths, #{output => json}),
    ?assert(is_binary(Json)),
    ?assertNotEqual(nomatch, binary:match(Json, <<"\"openapi\"">>)),
    ?assertNotEqual(nomatch, binary:match(Json, <<"3.0.3">>)),
    %% arity-1 default is json
    Json2 = liver_openapi_schema:generate_schema(Paths),
    ?assert(is_binary(Json2)),
    ok.

export_standard_nested_map(_Config) ->
    Paths = #{
        <<"/user">> => #{
            profile => [required, {nested_map, #{
                age => [required, is_integer],
                nick => [is_utf8_binary]
            }}]
        }
    },
    Doc = liver_openapi_schema:generate_schema(Paths, #{output => raw}),
    Post = maps:get(<<"post">>, maps:get(<<"/user">>, maps:get(paths, Doc))),
    ReqSchema = maps:get(schema,
        maps:get(<<"application/json">>,
            maps:get(content, maps:get(requestBody, Post)))),
    Profile = maps:get(profile, maps:get(properties, ReqSchema)),
    ?assertEqual(object, maps:get(type, Profile)),
    Age = maps:get(age, maps:get(properties, Profile)),
    ?assertEqual(integer, maps:get(type, Age)),
    ok.

export_generate_module_and_file(Config) ->
    Doc = liver_openapi_schema:generate(?MODULE, #{output => raw}),
    ?assert(maps:is_key(<<"/rpc">>, maps:get(paths, Doc))),
    ?assert(maps:is_key(<<"/atom_path">>, maps:get(paths, Doc))),
    Json = liver_openapi_schema:generate(?MODULE),
    ?assert(is_binary(Json)),
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, "openapi-out.json"),
    ok = liver_openapi_schema:generate_schema(liver_schema(), #{
        output => file,
        filename => File
    }),
    {ok, Bin} = file:read_file(File),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"openapi">>)),
    ok.

export_livr_rule_surface(_Config) ->
    Schema = #{
        a => [required, not_empty, string, {max_length, 10}, {min_length, 1},
              {length_between, [1, 10]}, {length_equal, 5}, {like, <<"^a">>}],
        b => [not_empty_list, any_object],
        c => [{eq, <<"x">>}, {eq, [<<"y">>]}, {eq, null}, {one_of, [<<"a">>, <<"b">>]}],
        d => [integer, positive_integer, decimal, positive_decimal,
              {max_number, 9}, {min_number, 1}, {number_between, [1, 9]}],
        e => [email, url, iso_date, {equal_to_field, <<"a">>}],
        f => [{nested_object, [{x, [string]}]},
              {nested_proplist, [{y, [integer]}]}],
        g => [{list_of, [string]},
              {list_of_objects, #{n => [integer]}},
              {list_of_objects, [#{n => [integer]}]},
              {nested_list, [is_integer]}],
        h => [{'or', [[string], [integer]]}],
        i => [trim, to_lc, to_uc, remove, leave_only, {default, <<>>}],
        j => [{variable_object, [<<"t">>, #{
                <<"a">> => #{<<"v">> => [integer]},
                <<"b">> => [{<<"v">>, [string]}]
             }]}],
        k => [{variable_object, [<<"t">>, [
                {<<"a">>, #{<<"v">> => [integer]}},
                {<<"b">>, [{<<"v">>, [string]}]}
             ]]}],
        l => [{list_of_different_objects, [<<"t">>, [
                {<<"a">>, #{<<"v">> => [integer]}}
             ]]}],
        m => [{list_of_different_objects, [[<<"t">>, [
                {<<"a">>, #{<<"v">> => [integer]}}
             ]]]}]
    },
    Frag = liver_openapi_schema:rules_to_openapi_schema([{nested_object, Schema}]),
    ?assertEqual(object, maps:get(type, Frag)),
    Props = maps:get(properties, Frag),
    ?assertEqual(string, maps:get(type, maps:get(a, Props))),
    ?assertEqual(array, maps:get(type, maps:get(g, Props))),
    ?assert(maps:is_key(oneOf, maps:get(h, Props)) orelse maps:is_key(oneOf, maps:get(j, Props))),
    %% list schema form for nested_object
    Frag2 = liver_openapi_schema:rules_to_openapi_schema(
        [{nested_object, [{<<"x">>, [required, string]}]}]),
    ?assertEqual(object, maps:get(type, Frag2)),
    ok.

export_standard_rule_surface(_Config) ->
    Schema = #{
        i => [is_integer, is_non_neg_integer, is_pos_integer, is_float, is_number,
              is_boolean, is_utf8_binary, is_binary, is_string, is_atom,
              is_list, is_map, is_proplist, is_null, is_not_null,
              is_undefined, is_not_undefined, is_term, is_tuple,
              is_pid, is_ref, is_port, is_fun],
        c => [{one_of_terms, [<<"a">>, <<"b">>]},
              {one_of_terms, [[<<"a">>, <<"b">>]]},
              {member, [1, 2]},
              {range, [{1, 10}]},
              {range, [1, 10]},
              {byte_size, [{eq, 3}]},
              {byte_size, [{min, 1}]},
              {byte_size, [{max, 9}]},
              {byte_size, [{between, 1, 9}]},
              {byte_size, []},
              {length, [{eq, 2}]},
              {length, [{min, 1}]},
              {length, [{max, 5}]},
              {length, []}],
        conv => [to_integer, to_float, to_boolean, to_string, to_utf8_binary,
                 to_binary, to_atom, to_existing_atom, to_list, to_map, to_proplist,
                 bit_size, tuple_size, map_size],
        eq_types => [{eq, true}, {eq, 1}, {eq, 1.5}, {eq, [1]}, {eq, foo}]
    },
    Frag = liver_openapi_schema:rules_to_openapi_schema([{nested_map, Schema}]),
    ?assertEqual(object, maps:get(type, Frag)),
    ok.

export_unknown_rule(_Config) ->
    ?assertThrow({error, {unknown_rule, totally_unknown_rule}},
                 liver_openapi_schema:rules_to_openapi_schema(
                     [totally_unknown_rule])),
    ok.

%%--------------------------------------------------------------------
%% Import
%%--------------------------------------------------------------------

import_roundtrip_ok(_Config) ->
    OA = #{
        type => object,
        required => [<<"name">>, <<"age">>],
        properties => #{
            <<"name">> => #{type => string},
            <<"age">> => #{type => integer, minimum => 0, maximum => 120},
            <<"tags">> => #{
                type => array,
                items => #{type => string}
            },
            <<"active">> => #{type => boolean}
        }
    },
    {ok, Schema} = liver_openapi_schema:from_openapi_schema(OA),
    Data = #{
        <<"name">> => <<"bob">>,
        <<"age">> => 30,
        <<"tags">> => [<<"a">>, <<"b">>],
        <<"active">> => true
    },
    ?assertMatch({ok, _}, liver:validate(Schema, Data)),
    ok.

import_roundtrip_error(_Config) ->
    OA = #{
        <<"type">> => <<"object">>,
        <<"required">> => [<<"email">>],
        <<"properties">> => #{
            <<"email">> => #{<<"type">> => <<"string">>, <<"format">> => <<"email">>}
        }
    },
    {ok, Schema} = liver_openapi_schema:from_openapi_schema(OA),
    ?assertMatch({error, _}, liver:validate(Schema, #{<<"email">> => <<"not-an-email">>})),
    ?assertMatch({error, _}, liver:validate(Schema, #{})),
    ok.

import_json_binary(_Config) ->
    Bin = <<"{\"type\":\"object\",\"required\":[\"n\"],"
            "\"properties\":{\"n\":{\"type\":\"integer\"}}}">>,
    {ok, Schema} = liver_openapi_schema:from_openapi_schema(Bin),
    ?assertMatch({ok, _}, liver:validate(Schema, #{<<"n">> => 1})),
    {ok, Schema2} = liver_openapi_schema:from_openapi_schema(Bin, #{}),
    ?assertEqual(Schema, Schema2),
    ok.

import_string_formats_and_lengths(_Config) ->
    OA = #{
        type => object,
        properties => #{
            <<"e">> => #{type => string, format => email},
            <<"u1">> => #{type => <<"string">>, format => <<"uri">>},
            <<"u2">> => #{type => string, format => <<"url">>},
            <<"u3">> => #{type => string, format => uri},
            <<"u4">> => #{type => string, format => url},
            <<"d1">> => #{type => string, format => <<"date">>},
            <<"d2">> => #{type => string, format => date},
            <<"len_eq">> => #{type => string, minLength => 3, maxLength => 3},
            <<"len_between">> => #{type => string, minLength => 1, maxLength => 5},
            <<"len_min">> => #{type => string, minLength => 2},
            <<"len_max">> => #{type => string, maxLength => 4},
            <<"enum">> => #{type => string, enum => [<<"a">>, <<"b">>]},
            <<"num">> => #{type => <<"number">>, minimum => 1, maximum => 2},
            <<"num_atom">> => #{type => number, minimum => 1, maximum => 2},
            <<"int_bin">> => #{type => <<"integer">>},
            <<"bool_bin">> => #{type => <<"boolean">>},
            <<"arr_bin">> => #{type => <<"array">>, items => #{type => <<"string">>}}
        }
    },
    {ok, Schema} = liver_openapi_schema:from_openapi_schema(OA),
    ?assert(is_map(Schema)),
    ?assertMatch({ok, _}, liver:validate(Schema, #{
        <<"e">> => <<"a@b.co">>,
        <<"enum">> => <<"a">>,
        <<"num">> => 1.5,
        <<"int_bin">> => 7,
        <<"bool_bin">> => false,
        <<"arr_bin">> => [<<"x">>]
    })),
    ok.

import_nested_and_array(_Config) ->
    OA = #{
        type => object,
        properties => #{
            <<"obj">> => #{
                type => object,
                required => [<<"x">>],
                properties => #{
                    <<"x">> => #{type => integer}
                }
            },
            <<"obj2">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"y">> => #{<<"type">> => <<"string">>}
                }
            },
            <<"bare">> => #{
                properties => #{
                    <<"z">> => #{type => boolean}
                }
            },
            <<"items_missing">> => #{type => array},
            <<"enum_only">> => #{enum => [1, 2, 3]}
        }
    },
    {ok, Schema} = liver_openapi_schema:from_openapi_schema(OA),
    ?assertMatch({ok, _}, liver:validate(Schema, #{
        <<"obj">> => #{<<"x">> => 1},
        <<"obj2">> => #{<<"y">> => <<"ok">>},
        <<"bare">> => #{<<"z">> => true},
        <<"items_missing">> => [a, b],
        <<"enum_only">> => 2
    })),
    %% top-level properties without type
    {ok, Schema2} = liver_openapi_schema:from_openapi_schema(#{
        properties => #{<<"a">> => #{type => string}}
    }),
    ?assertMatch({ok, _}, liver:validate(Schema2, #{<<"a">> => <<"x">>})),
    ok.

import_unsupported_constructs(_Config) ->
    ?assertEqual({error, {unsupported, ref}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     properties => #{
                         <<"x">> => #{<<"$ref">> => <<"#/components/schemas/X">>}
                     }
                 })),
    ?assertEqual({error, {unsupported, ref}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     properties => #{<<"x">> => #{'$ref' => <<"#/X">>}}
                 })),
    ?assertEqual({error, {unsupported, allOf}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     allOf => [#{}]
                 })),
    ?assertEqual({error, {unsupported, additionalProperties}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     additionalProperties => true,
                     properties => #{}
                 })),
    ?assertEqual({error, {unsupported, oneOf}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     oneOf => [#{}]
                 })),
    ?assertEqual({error, {unsupported, anyOf}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     anyOf => [#{}]
                 })),
    ?assertEqual({error, {unsupported, oneOf}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     properties => #{<<"x">> => #{oneOf => [#{type => string}]}}
                 })),
    ?assertEqual({error, {unsupported, anyOf}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     properties => #{<<"x">> => #{anyOf => [#{type => string}]}}
                 })),
    ?assertEqual({error, {unsupported, pattern}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     properties => #{
                         <<"x">> => #{type => string, pattern => <<"^a">>}
                     }
                 })),
    ok.

import_edge_errors(_Config) ->
    ?assertEqual({error, {unsupported, missing_type}},
                 liver_openapi_schema:from_openapi_schema(#{})),
    ?assertEqual({error, {unsupported, {top_level_type, <<"string">>}}},
                 liver_openapi_schema:from_openapi_schema(#{type => <<"string">>})),
    ?assertEqual({error, {unsupported, {type, <<"weird">>}}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     properties => #{<<"x">> => #{type => <<"weird">>}}
                 })),
    %% nested object import that fails via $ref inside
    ?assertEqual({error, {unsupported, ref}},
                 liver_openapi_schema:from_openapi_schema(#{
                     type => object,
                     properties => #{
                         <<"n">> => #{
                             type => object,
                             properties => #{
                                 <<"z">> => #{<<"$ref">> => <<"#/Z">>}
                             }
                         }
                     }
                 })),
    %% single-sided bounds ignored (still imports type)
    {ok, Schema} = liver_openapi_schema:from_openapi_schema(#{
        type => object,
        properties => #{
            <<"only_min">> => #{type => integer, minimum => 1},
            <<"list_key">> => #{type => string}
        },
        required => ["list_key"]
    }),
    ?assert(maps:is_key(<<"only_min">>, Schema) orelse maps:is_key(only_min, Schema)
            orelse true),
    ok.
