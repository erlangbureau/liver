-module(livr_rules_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("cases/livr/cases.hrl").

all() ->
    [
        like_end_of_value,
        {group, maps_positive},
        {group, maps_negative},
        {group, proplists_positive},
        {group, proplists_negative},
        {group, maps_aliases_positive},
        {group, maps_aliases_negative},
        {group, proplists_aliases_positive},
        {group, proplists_aliases_negative}
    ].

groups() ->
    [
        {maps_positive, [parallel], ?LIVR_POSITIVE_CASES},
        {maps_negative, [parallel], ?LIVR_NEGATIVE_CASES},
        {proplists_positive, [parallel], ?LIVR_POSITIVE_CASES},
        {proplists_negative, [parallel], ?LIVR_NEGATIVE_CASES},
        {maps_aliases_positive, [parallel], ?LIVR_ALIASES_POSITIVE_CASES},
        {maps_aliases_negative, [parallel], ?LIVR_ALIASES_NEGATIVE_CASES},
        {proplists_aliases_positive, [parallel], ?LIVR_ALIASES_POSITIVE_CASES},
        {proplists_aliases_negative, [parallel], ?LIVR_ALIASES_NEGATIVE_CASES}
    ].

init_per_suite(Config) ->
    [{cases_suite, livr}, {validate_opts, #{rule_set => livr_spec}} | Config].

end_per_suite(Config) ->
    Config.

init_per_group(maps_positive, Config) ->
    [{data_form, maps}, {init_type, positive} | Config];
init_per_group(maps_negative, Config) ->
    [{data_form, maps}, {init_type, negative} | Config];
init_per_group(proplists_positive, Config) ->
    [{data_form, proplists}, {init_type, positive} | Config];
init_per_group(proplists_negative, Config) ->
    [{data_form, proplists}, {init_type, negative} | Config];
init_per_group(maps_aliases_positive, Config) ->
    [{data_form, maps}, {init_type, aliases_positive} | Config];
init_per_group(maps_aliases_negative, Config) ->
    [{data_form, maps}, {init_type, aliases_negative} | Config];
init_per_group(proplists_aliases_positive, Config) ->
    [{data_form, proplists}, {init_type, aliases_positive} | Config];
init_per_group(proplists_aliases_negative, Config) ->
    [{data_form, proplists}, {init_type, aliases_negative} | Config].

end_per_group(_Name, Config) ->
    Config2 = lists:keydelete(init_type, 1, Config),
    lists:keydelete(data_form, 1, Config2).

%% like: $ matches only at the end of the value, as in JavaScript
like_end_of_value(_Config) ->
    Opts = #{rule_set => livr_spec},
    Zip = <<"^[0-9]{5}$">>,
    ?assertEqual({ok, #{z => <<"12345">>}}, liver:validate(#{z => [{like, Zip}]}, #{z => <<"12345">>}, Opts)),
    ?assertEqual({error, #{z => <<"WRONG_FORMAT">>}}, liver:validate(#{z => [{like, Zip}]}, #{z => <<"12345\n">>}, Opts)),
    ?assertEqual({error, #{z => <<"WRONG_FORMAT">>}}, liver:validate(#{z => [{like, [Zip, <<"i">>]}]}, #{z => <<"12345\n">>}, Opts)),
    ?assertEqual({error, #{z => <<"WRONG_FORMAT">>}}, liver:validate(#{z => [{like, Zip}]}, #{z => <<"12345\n<script>">>}, Opts)).

-include("cases/livr/runners.hrl").
