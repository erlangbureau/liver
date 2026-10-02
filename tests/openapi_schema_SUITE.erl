-module(openapi_schema_SUITE).

%% Export liver schemas to OpenAPI and import Schema Objects to erlang_standard.

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        export_livr_path_schema,
        export_standard_nested_map,
        import_roundtrip_ok,
        import_roundtrip_error,
        import_unsupported_ref
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
    ?assertMatch(#{<<"openapi">> := <<"3.0.3">>}, json:decode(Json)),
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

import_unsupported_ref(_Config) ->
    OA = #{
        type => object,
        properties => #{
            <<"x">> => #{<<"$ref">> => <<"#/components/schemas/X">>}
        }
    },
    ?assertEqual({error, {unsupported, ref}},
                 liver_openapi_schema:from_openapi_schema(OA)),
    ok.
