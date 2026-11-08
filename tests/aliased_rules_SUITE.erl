-module(aliased_rules_SUITE).

%% LIVR aliased rules: liver:add_aliased_rule/1 and the aliased_rules option.
%% The LIVR aliases_* cases run in livr_rules_SUITE.

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LIVR, #{rule_set => livr_spec}).

all() ->
    [
        registered_and_per_call,
        per_call_overrides_registered,
        alias_errors,
        missing_field,
        nested,
        json_keys,
        erlang_standard,
        list_schema,
        return_proplist,
        invalid_alias
    ].

init_per_suite(Config) ->
    _ = application:load(liver),
    Config.

end_per_suite(Config) ->
    persistent_term:erase(liver_aliased_rules),
    Config.

%% Same as LIVR's js-validator-livr t/tests-sync/04-custom_aliases.js:
%% a registered alias and a per-call one, each with its own error.
registered_and_per_call(_Config) ->
    ok = liver:add_aliased_rule(#{
        name => strong_password1,
        rules => #{min_length => 8},
        error => <<"WEAK_PASSWORD1">>
    }),
    Opts = livr([#{
        name => strong_password2,
        rules => #{min_length => 8},
        error => <<"WEAK_PASSWORD2">>
    }]),
    Schema = #{password1 => strong_password1, password2 => strong_password2},
    ?assertEqual({error, #{password1 => <<"WEAK_PASSWORD1">>,
                           password2 => <<"WEAK_PASSWORD2">>}},
                 liver:validate(Schema, #{password1 => <<"mypass">>,
                                          password2 => <<"mypass">>}, Opts)),
    %% proplist form of the option and the alias
    PlOpts = [{rule_set, livr_spec}, {aliased_rules, [[
        {name, strong_password2},
        {rules, [{min_length, 8}]},
        {error, <<"WEAK_PASSWORD2">>}
    ]]}],
    ?assertEqual({error, [{password1, <<"WEAK_PASSWORD1">>},
                          {password2, <<"WEAK_PASSWORD2">>}]},
                 liver:validate([{password1, strong_password1},
                                 {password2, strong_password2}],
                                [{password1, <<"mypass">>},
                                 {password2, <<"mypass">>}], PlOpts)),
    ok.

per_call_overrides_registered(_Config) ->
    ok = liver:add_aliased_rule(#{name => short_code, rules => #{max_length => 2}}),
    ?assertEqual({error, #{c => <<"TOO_LONG">>}},
                 liver:validate(#{c => short_code}, #{c => <<"abc">>}, ?LIVR)),
    Opts = livr([#{name => short_code, rules => #{max_length => 3}}]),
    ?assertEqual({ok, #{c => <<"abc">>}},
                 liver:validate(#{c => short_code}, #{c => <<"abc">>}, Opts)),
    ok.

alias_errors(_Config) ->
    Opts = livr([
        #{name => adult_age, rules => [positive_integer, #{min_number => 18}]},
        #{name => adult_age2, rules => [positive_integer, #{min_number => 18}],
          error => <<"WRONG_AGE">>}
    ]),
    %% without error: the errors of the alias rules
    ?assertEqual({error, #{a => <<"TOO_LOW">>, b => <<"NOT_POSITIVE_INTEGER">>}},
                 liver:validate(#{a => adult_age, b => [adult_age]},
                                #{a => 15, b => -1}, Opts)),
    %% with error: it replaces them
    ?assertEqual({error, #{a => <<"WRONG_AGE">>, b => <<"WRONG_AGE">>}},
                 liver:validate(#{a => adult_age2, b => #{adult_age2 => []}},
                                #{a => 15, b => -1}, Opts)),
    %% the output of the alias rules is the field value
    ?assertEqual({ok, #{a => 20, b => 30}},
                 liver:validate(#{a => adult_age, b => [required, adult_age2]},
                                #{a => <<"20">>, b => 30}, Opts)),
    ok.

missing_field(_Config) ->
    Opts = livr([
        #{name => adult_age, rules => [positive_integer, #{min_number => 18}]},
        #{name => req_email, rules => [required, email]},
        #{name => req_email2, rules => [required, email], error => <<"BAD_EMAIL">>},
        #{name => role, rules => [#{default => <<"user">>}, #{one_of => [[<<"user">>, <<"admin">>]]}]}
    ]),
    %% alias without required: the field stays absent
    ?assertEqual({ok, #{}}, liver:validate(#{a => adult_age}, #{}, Opts)),
    %% required inside the alias
    ?assertEqual({error, #{a => <<"REQUIRED">>, b => <<"BAD_EMAIL">>}},
                 liver:validate(#{a => req_email, b => req_email2}, #{}, Opts)),
    %% default inside the alias
    ?assertEqual({ok, #{r => <<"user">>}},
                 liver:validate(#{r => role}, #{}, Opts)),
    ok.

nested(_Config) ->
    Opts = livr([
        #{name => adult_age, rules => [positive_integer, #{min_number => 18}]},
        #{name => user, rules => #{nested_object => #{
            name => required,
            age => adult_age
        }}, error => <<"WRONG_USER">>},
        #{name => users, rules => #{list_of => [user]}}
    ]),
    %% in nested_object, list_of and 'or'
    Schema = #{
        owner => #{nested_object => #{age => adult_age}},
        ages => #{list_of => adult_age},
        id => #{'or' => [[adult_age], [email]]}
    },
    ?assertEqual({ok, #{owner => #{age => 20}, ages => [18, 19], id => <<"a@b.co">>}},
                 liver:validate(Schema, #{owner => #{age => <<"20">>},
                                          ages => [18, <<"19">>],
                                          id => <<"a@b.co">>}, Opts)),
    ?assertEqual({error, #{owner => #{age => <<"TOO_LOW">>},
                           ages => [null, <<"TOO_LOW">>],
                           id => <<"WRONG_EMAIL">>}},
                 liver:validate(Schema, #{owner => #{age => 3},
                                          ages => [18, 3],
                                          id => <<"x">>}, Opts)),
    %% aliases in aliases, the custom error of the inner alias
    ?assertEqual({error, #{u => [null, <<"WRONG_USER">>]}},
                 liver:validate(#{u => users},
                                #{u => [#{name => <<"Ann">>, age => 30},
                                        #{name => <<"Bob">>, age => 3}]}, Opts)),
    ok.

json_keys(_Config) ->
    %% an alias as decoded from LIVR's aliases.json (binary keys and names)
    ok = liver:add_aliased_rule(#{
        <<"name">> => <<"valid_address">>,
        <<"rules">> => #{<<"nested_object">> => #{
            <<"country">> => <<"required">>,
            <<"zip">> => <<"positive_integer">>
        }},
        <<"error">> => <<"WRONG_ADDRESS">>
    }),
    Schema = #{<<"address">> => [<<"required">>, <<"valid_address">>]},
    ?assertEqual({ok, #{<<"address">> => #{<<"country">> => <<"UA">>, <<"zip">> => 1001}}},
                 liver:validate(Schema, #{<<"address">> => #{<<"country">> => <<"UA">>,
                                                             <<"zip">> => <<"1001">>}}, ?LIVR)),
    ?assertEqual({error, #{<<"address">> => <<"WRONG_ADDRESS">>}},
                 liver:validate(Schema, #{<<"address">> => #{<<"zip">> => <<"1001">>}}, ?LIVR)),
    ok.

erlang_standard(_Config) ->
    %% alias rules use the rule set of the call
    Opts = #{aliased_rules => [
        #{name => id, rules => [required, is_pos_integer]},
        #{name => id2, rules => [required, is_pos_integer], error => bad_id}
    ]},
    ?assertEqual({ok, #{a => 1}}, liver:validate(#{a => id}, #{a => 1}, Opts)),
    ?assertEqual({error, #{a => not_pos_integer, b => bad_id}},
                 liver:validate(#{a => id, b => id2}, #{a => 0, b => 0}, Opts)),
    ?assertEqual({error, #{m => #{a => bad_id}}},
                 liver:validate(#{m => {nested_map, #{a => id2}}}, #{m => #{a => 0}}, Opts)),
    ok.

list_schema(_Config) ->
    %% a list of rules as the root schema validates a list
    Opts = livr([
        #{name => adult_age, rules => [positive_integer, #{min_number => 18}]}
    ]),
    ?assertEqual({ok, [20, 30]}, liver:validate([adult_age], [20, <<"30">>], Opts)),
    ok.

return_proplist(_Config) ->
    Opts = #{rule_set => livr_spec, return => proplist, aliased_rules => [
        #{name => adult_age, rules => [positive_integer, #{min_number => 18}]},
        #{name => role, rules => #{default => <<"user">>}},
        #{name => req, rules => required}
    ]},
    %% a proplist schema, so that the order of the result is fixed
    ?assertEqual({ok, [{a, 20}, {r, <<"user">>}]},
                 liver:validate([{a, adult_age}, {r, role}], #{a => <<"20">>}, Opts)),
    ?assertEqual({error, [{a, <<"TOO_LOW">>}, {q, <<"REQUIRED">>}]},
                 liver:validate([{a, adult_age}, {q, req}], #{a => 3}, Opts)),
    ok.

invalid_alias(_Config) ->
    ?assertError({invalid_aliased_rule, _},
                 liver:add_aliased_rule(#{rules => required})),
    ?assertError({invalid_aliased_rule, _},
                 liver:add_aliased_rule(#{name => no_rules})),
    ok.

%% internal
livr(Aliases) ->
    #{rule_set => livr_spec, aliased_rules => Aliases}.
