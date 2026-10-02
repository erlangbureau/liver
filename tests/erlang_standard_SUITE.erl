-module(erlang_standard_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("cases/erlang_standard/cases.hrl").

all() ->
    [
        {group, maps_positive},
        {group, maps_negative},
        {group, proplists_positive},
        {group, proplists_negative}
    ].

groups() ->
    [
        {maps_positive, [parallel], ?ERLANG_STANDARD_POSITIVE_CASES},
        {maps_negative, [parallel], ?ERLANG_STANDARD_NEGATIVE_CASES},
        {proplists_positive, [parallel], ?ERLANG_STANDARD_POSITIVE_CASES},
        {proplists_negative, [parallel], ?ERLANG_STANDARD_NEGATIVE_CASES}
    ].

init_per_suite(Config) ->
    [{cases_suite, erlang_standard},
     {validate_opts, #{rule_set => erlang_standard}} | Config].

end_per_suite(Config) ->
    Config.

init_per_group(maps_positive, Config) ->
    [{data_form, maps}, {init_type, positive} | Config];
init_per_group(maps_negative, Config) ->
    [{data_form, maps}, {init_type, negative} | Config];
init_per_group(proplists_positive, Config) ->
    [{data_form, proplists}, {init_type, positive} | Config];
init_per_group(proplists_negative, Config) ->
    [{data_form, proplists}, {init_type, negative} | Config].

end_per_group(_Name, Config) ->
    Config2 = lists:keydelete(init_type, 1, Config),
    lists:keydelete(data_form, 1, Config2).

-include("cases/erlang_standard/runners.hrl").
