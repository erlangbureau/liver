-module(livr25_rules_SUITE).

%% The LIVR 2.5 test suite (tests/cases/livr25, imported from
%% test_suite_2.5 of the LIVR repository by scripts/livr_import.erl).
%% The cases with options.json fix the clock, see liver_clock.

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("cases/livr25/cases.hrl").

all() ->
    [
        string_form,
        clock,
        schema_error_at_validate,
        unknown_rule_at_validate,
        null_argument,
        eq_one_of_booleans,
        unique_key_value_format,
        url_port,
        {group, maps_positive},
        {group, maps_negative},
        {group, proplists_positive},
        {group, proplists_negative},
        {group, maps_aliases_positive},
        {group, maps_aliases_negative},
        {group, proplists_aliases_positive},
        {group, proplists_aliases_negative},
        {group, maps_schema_errors},
        {group, proplists_schema_errors}
    ].

%% Not parallel: a case may fix the clock.
groups() ->
    [
        {maps_positive, [], ?LIVR_POSITIVE_CASES},
        {maps_negative, [], ?LIVR_NEGATIVE_CASES},
        {proplists_positive, [], ?LIVR_POSITIVE_CASES},
        {proplists_negative, [], ?LIVR_NEGATIVE_CASES},
        {maps_aliases_positive, [], ?LIVR_ALIASES_POSITIVE_CASES},
        {maps_aliases_negative, [], ?LIVR_ALIASES_NEGATIVE_CASES},
        {proplists_aliases_positive, [], ?LIVR_ALIASES_POSITIVE_CASES},
        {proplists_aliases_negative, [], ?LIVR_ALIASES_NEGATIVE_CASES},
        {maps_schema_errors, [parallel], ?LIVR_SCHEMA_ERRORS_CASES},
        {proplists_schema_errors, [parallel], ?LIVR_SCHEMA_ERRORS_CASES}
    ].

init_per_suite(Config) ->
    [{cases_suite, livr25}, {validate_opts, #{rule_set => livr_spec}} | Config].

end_per_suite(Config) ->
    ok = liver_clock:set_time(undefined),
    Config.

init_per_group(Group, Config) ->
    [Form, Kind] = string:split(atom_to_list(Group), "_"),
    [{data_form, list_to_atom(Form)}, {init_type, list_to_atom(Kind)} | Config].

%% A proplist schema whose field names are all rule names is a list of
%% rules for liver: [{email, 'when'}] is the rule email with an argument.
init_per_testcase(Case, Config)
        when Case =:= when_without_arguments; Case =:= when_one_argument ->
    case ?config(data_form, Config) of
        proplists -> {skip, "the field name email is a rule name"};
        maps -> Config
    end;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

end_per_group(_Name, Config) ->
    Config2 = lists:keydelete(init_type, 1, Config),
    lists:keydelete(data_form, 1, Config2).

-define(LIVR, #{rule_set => livr_spec}).

%% 2.2: the test vectors of the string form
string_form(_Config) ->
    Vectors = [{1.0, <<"1">>}, {-0.0, <<"0">>}, {100, <<"100">>}, {1.0e3, <<"1000">>},
               {0.1, <<"0.1">>}, {-12.25, <<"-12.25">>},
               {123456789012345, <<"123456789012345">>},
               {1.5e-7, <<"0.00000015">>}, {1.0e-6, <<"0.000001">>},
               {-2.5e-7, <<"-0.00000025">>}, {123.456, <<"123.456">>}],
    [?assertEqual({ok, #{v => [Expected]}},
                  liver:validate(#{v => {split, <<"|">>}}, #{v => Number}, ?LIVR))
     || {Number, Expected} <- Vectors],
    ok.

%% today, yesterday and tomorrow are in UTC, from liver_clock
clock(_Config) ->
    Schema = #{d => {iso_date, #{<<"min">> => <<"today">>, <<"max">> => <<"tomorrow">>}}},
    Ms = calendar:rfc3339_to_system_time("2026-10-18T23:30:00-02:00", [{unit, millisecond}]),
    ok = liver_clock:set_time(Ms),
    ?assertEqual({ok, #{d => {2026, 10, 19}}}, liver:validate(Schema, #{d => <<"2026-10-19">>}, ?LIVR)),
    ?assertEqual({error, #{d => <<"DATE_TOO_EARLY">>}},
                 liver:validate(Schema, #{d => <<"2026-10-18">>}, ?LIVR)),
    ?assertEqual({error, #{d => <<"DATE_TOO_LATE">>}},
                 liver:validate(Schema, #{d => <<"2026-10-21">>}, ?LIVR)).

%% validate/3 raises the schema error of a rule that runs
schema_error_at_validate(_Config) ->
    ?assertError({invalid_args, boolean, 1},
                 liver:validate(#{b => {boolean, 1}}, #{b => true}, ?LIVR)),
    ?assertEqual({error, {invalid_args, boolean, 1}},
                 liver:check_schema(#{b => {boolean, 1}}, ?LIVR)),
    ?assertEqual({error, {unknown_rule, no_such_rule}},
                 liver:check_schema(#{b => [required, no_such_rule]}, ?LIVR)),
    ?assertEqual(ok, liver:check_schema(#{b => [required, boolean]}, ?LIVR)).

%% 2.6: an unknown rule is a schema error of validate/3, also when no
%% data reaches it; the schemas that passed are checked again after a
%% rule, a rule set or an aliased rule is registered
unknown_rule_at_validate(_Config) ->
    Unknown = {unknown_rule, no_such_rule},
    %% top level: the field present and missing
    ?assertError(Unknown, liver:validate(#{a => no_such_rule}, #{a => 1}, ?LIVR)),
    ?assertError(Unknown, liver:validate(#{a => [required, no_such_rule]}, #{}, ?LIVR)),
    ?assertError(Unknown, liver:validate(#{a => no_such_rule}, #{}, ?LIVR)),
    %% nested, in a list and in an 'or' branch that does not run
    ?assertError(Unknown, liver:validate(#{a => {nested_object, #{b => no_such_rule}}},
                                         #{a => #{}}, ?LIVR)),
    ?assertError(Unknown, liver:validate(#{a => {list_of, [no_such_rule]}}, #{a => []}, ?LIVR)),
    ?assertError(Unknown, liver:validate(#{a => {list_of_objects, #{b => no_such_rule}}},
                                         #{}, ?LIVR)),
    %% a rule list with an unknown rule is a proplist schema for liver, a wrong one
    ?assertError({invalid_schema, [integer, no_such_rule]},
                 liver:validate([integer, no_such_rule], 1, ?LIVR)),
    ?assertError(Unknown, liver:validate(#{a => {'or', [[integer], [no_such_rule]]}},
                                         #{a => 1}, ?LIVR)),
    %% through an aliased rule, registered and per call
    ok = liver:add_aliased_rule(#{name => livr25_broken_alias, rules => [integer, no_such_rule]}),
    ?assertError(Unknown, liver:validate(#{a => livr25_broken_alias}, #{}, ?LIVR)),
    Alias = #{name => per_call_alias, rules => [no_such_rule]},
    ?assertError(Unknown, liver:validate(#{a => per_call_alias}, #{a => 1},
                                         ?LIVR#{aliased_rules => [Alias]})),
    %% the same schema twice (the second from the cache), a wrong argument too
    Schema = #{a => [required, integer]},
    ?assertEqual({ok, #{a => 1}}, liver:validate(Schema, #{a => 1}, ?LIVR)),
    ?assertEqual({error, #{a => <<"REQUIRED">>}}, liver:validate(Schema, #{}, ?LIVR)),
    ?assertError({invalid_args, boolean, 1}, liver:validate(#{a => {boolean, 1}}, #{}, ?LIVR)),
    %% a rule registered later
    Opts = #{rule_set => [livr_spec, livr25_later]},
    ok = liver:add_rule_set(livr25_later, #{}),
    ?assertError({unknown_rule, later_rule}, liver:validate(#{a => later_rule}, #{}, Opts)),
    ok = liver:add_rule_set(livr25_later, #{later_rule => ?MODULE}),
    ?assertEqual({ok, #{}}, liver:validate(#{a => later_rule}, #{}, Opts)),
    ?assertEqual({ok, #{a => {args, []}}}, liver:validate(#{a => later_rule}, #{a => 1}, Opts)),
    ok.

later_rule(Args, _Value, _Opts) ->
    {ok, {args, Args}}.

%% 2.6: {"rule": null} is one null argument, a schema error for the 2.5
%% rules; iso_date (2.0 ignored its arguments), the other 2.0 rules and
%% custom rules keep the 2.0 handling
null_argument(_Config) ->
    ?assertError({invalid_args, boolean, null},
                 liver:validate(#{b => {boolean, null}}, #{b => <<"1">>}, ?LIVR)),
    Uuid = <<"6ba7b810-9dad-41d1-80b4-00c04fd430c8">>,
    ?assertError({invalid_args, uuid, null},
                 liver:validate(#{u => {uuid, null}}, #{u => Uuid}, ?LIVR)),
    [?assertMatch({error, {invalid_args, Rule, null}},
                  liver:check_schema(#{f => {Rule, null}}, ?LIVR))
     || Rule <- [boolean, not_one_of, max_decimals, digits, starts_with, ends_with,
                 contains, iso_datetime, iso_time, required_if, 'when', values, keys,
                 any, list_length, unique, split, uuid, ip, cidr, hostname, base64,
                 hex, digest, mongo_id, hex_color, mac, e164, semver, jwt, luhn,
                 credit_card, iban]],
    %% iso_date: null is the plain iso_date
    ?assertEqual({ok, #{d => {2026, 1, 2}}},
                 liver:validate(#{d => {iso_date, null}}, #{d => <<"2026-01-02">>}, ?LIVR)),
    ?assertEqual(ok, liver:check_schema(#{d => {iso_date, null}}, ?LIVR)),
    %% null inside a list of arguments is a wrong value
    [?assertMatch({error, {invalid_args, Rule, Args}},
                  liver:check_schema(#{f => {Rule, Args}}, ?LIVR))
     || {Rule, Args} <- [{boolean, [null]}, {iso_date, [null]}, {not_one_of, [null]},
                         {required_if, #{<<"type">> => null}}, {list_length, #{<<"min">> => null}}]],
    %% 2.0: default sets null
    ?assertEqual({ok, #{v => null}}, liver:validate(#{v => {default, null}}, #{}, ?LIVR)),
    %% a custom rule gets null
    ok = liver:add_rule_set(livr25_custom, #{echo_args => ?MODULE}),
    Opts = #{rule_set => [livr_spec, livr25_custom]},
    ?assertEqual({ok, #{v => {args, null}}},
                 liver:validate(#{v => {echo_args, null}}, #{v => <<"x">>}, Opts)),
    ?assertEqual(ok, liver:check_schema(#{v => {echo_args, null}}, Opts)).

echo_args(Args, _Value, _Opts) ->
    {ok, {args, Args}}.

%% 2.3: eq and one_of accept "true" and "false" for a boolean, and the
%% boolean itself as in 2.0 (liver 2.0 matched no other string form)
eq_one_of_booleans(_Config) ->
    Ok = fun(Rule, Value) ->
        ?assertEqual({ok, #{v => Value}}, liver:validate(#{v => Rule}, #{v => Value}, ?LIVR))
    end,
    Err = fun(Rule, Value) ->
        ?assertEqual({error, #{v => <<"NOT_ALLOWED_VALUE">>}},
                     liver:validate(#{v => Rule}, #{v => Value}, ?LIVR))
    end,
    %% the 2.0 form: the same boolean
    Ok({eq, true}, true),
    Ok({eq, false}, false),
    Ok({one_of, [[true, false]]}, false),
    Ok({one_of, [<<"a">>, true]}, true),
    %% 2.5: the string form, both directions
    Ok({eq, true}, <<"true">>),
    Ok({eq, false}, <<"false">>),
    Ok({eq, <<"true">>}, true),
    Ok({one_of, [[true, false]]}, <<"false">>),
    Ok({one_of, [<<"yes">>, <<"false">>]}, false),
    Err({eq, true}, false),
    Err({eq, true}, <<"false">>),
    Err({eq, true}, <<"1">>),
    Err({eq, true}, <<"True">>),
    Err({eq, <<"1">>}, true),
    Err({one_of, [[true]]}, <<"TRUE">>).

%% unique with keys: an object or a list as a key value is FORMAT_ERROR,
%% also when another key of the item is empty
unique_key_value_format(_Config) ->
    Schema = #{v => {unique, [<<"a">>, <<"b">>]}},
    [?assertEqual({error, #{v => <<"FORMAT_ERROR">>}},
                  liver:validate(Schema, #{v => [Item]}, ?LIVR))
     || Item <- [#{<<"a">> => <<>>, <<"b">> => [1]},
                 #{<<"a">> => null, <<"b">> => #{}},
                 #{<<"b">> => [{<<"x">>, 1}]},
                 [{<<"a">>, [1]}, {<<"b">>, <<>>}]]],
    ?assertEqual({ok, #{v => [#{<<"a">> => <<>>, <<"b">> => 1}]}},
                 liver:validate(Schema, #{v => [#{<<"a">> => <<>>, <<"b">> => 1}]}, ?LIVR)).

%% url: a port has 1-5 ASCII digits and a value of 0-65535, also with user
%% info, a path, a query or a fragment after it; uri_string of OTP 25 and 26
%% accepts more digits
url_port(_Config) ->
    [?assertEqual({ok, #{u => Url}}, liver:validate(#{u => url}, #{u => Url}, ?LIVR))
     || Url <- [<<"http://example.com:0">>, <<"http://example.com:00080/">>,
                <<"https://user:pass@example.com:8?q=1">>, <<"http://u@example.com:65535#top">>,
                <<"http://user:pass@example.com/">>]],
    [?assertEqual({error, #{u => <<"WRONG_URL">>}}, liver:validate(#{u => url}, #{u => Url}, ?LIVR))
     || Url <- [<<"http://example.com:">>, <<"http://example.com:/path">>,
                <<"http://example.com:000080">>, <<"http://example.com:123456/">>,
                <<"http://user:pass@example.com:65536/">>, <<"https://u@example.com:99999?q=1">>]].

-include("cases/livr25/runners.hrl").
