-module(unit_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LIVR_TEST_PATH, "deps/LIVR/test_suite/").

positive_test() ->
    PositiveRulesPath = ?LIVR_TEST_PATH ++ "/positive",
    {ok, ListPathOfRules} = file:list_dir(PositiveRulesPath),
    [begin
        Path = PositiveRulesPath ++ "/" ++ PathOfRule,
        {Input, Output, Rules} = get_conditions(Path, positive),
        ?debugFmt("TEST: ~p; RULE: ~p", [positive, PathOfRule]),
        Result = oliver:validate(Rules, Input),
        ?debugFmt("Positive test rules: ~p~n; Input: ~p~n; Output: ~p~n; "
                  "Result: ~p", [Rules, Input, Output, Result]),
        ?assertEqual({ok, Output}, Result)
    end || PathOfRule <- ListPathOfRules].

negative_test() ->
    PositiveRulesPath = ?LIVR_TEST_PATH ++ "/negative",
    {ok, ListPathOfRules} = file:list_dir(PositiveRulesPath),
    [begin
        Path = PositiveRulesPath ++ "/" ++ PathOfRule,
        {Input, Rules, Errors} = get_conditions(Path, negative),
        ?debugFmt("TEST: ~p; RULE: ~p", [negative, PathOfRule]),
        Result = oliver:validate(Rules, Input),
        ?debugFmt("Positive test rules: ~p~n; Input: ~p~n; Errors: ~p~n; "
                  "Result: ~p", [Rules, Input, Errors, Result]),
        ?assertEqual({error, Errors}, Result)
    end || PathOfRule <- ListPathOfRules].

aliases_positive_test() ->
    PositiveRulesPath = ?LIVR_TEST_PATH ++ "/aliases_positive",
    {ok, ListPathOfRules} = file:list_dir(PositiveRulesPath),
    [begin
        Path = PositiveRulesPath ++ "/" ++ PathOfRule,
        {Aliases, Input, Output, Rules} = get_conditions(Path, aliases_positive),
        ?debugFmt("TEST: ~p; RULE: ~p", [aliase_positive, PathOfRule]),
        %ok = oliver:add_alias(Aliases),
        Result = oliver:validate(Rules, Input),
        ?debugFmt("Positive test rules: ~p~n; Input: ~p~n; Aliases: ~p~n; "
                  "Result: ~p", [Rules, Input, Aliases, Result]),
        ?assertEqual({ok, Output}, Result)
    end || PathOfRule <- ListPathOfRules].

aliases_negative_test() ->
    PositiveRulesPath = ?LIVR_TEST_PATH ++ "/aliases_negative",
    {ok, ListPathOfRules} = file:list_dir(PositiveRulesPath),
    [begin
        Path = PositiveRulesPath ++ "/" ++ PathOfRule,
        {Aliases, Input, Rules, Errors} = get_conditions(Path, aliases_negative),
        ?debugFmt("TEST: ~p; RULE: ~p", [aliases_negative, PathOfRule]),
        %ok = oliver:add_alias(Aliases),
        Result = oliver:validate(Rules, Input),
        ?debugFmt("Positive test rules: ~p~n; Input: ~p~n; Errors: ~p~n; "
                  "Result: ~p", [Rules, Input, Errors, Result]),
        ?assertEqual({error, Errors}, Result)
    end || PathOfRule <- ListPathOfRules].

get_conditions(Path, positive) ->
    {ok, Input} = file:read_file(Path ++ "/input.json"),
    {ok, Output} = file:read_file(Path ++ "/output.json"),
    {ok, Rules} = file:read_file(Path ++ "/rules.json"),
    {decode(Input), decode(Output), decode(Rules)};
get_conditions(Path, negative) ->
    {ok, Input} = file:read_file(Path ++ "/input.json"),
    {ok, Rules} = file:read_file(Path ++ "/rules.json"),
    {ok, Errors} = file:read_file(Path ++ "/errors.json"),
    {decode(Input), decode(Rules), decode(Errors)};
get_conditions(Path, aliases_positive) ->
    {ok, Aliases} = file:read_file(Path ++ "/aliases.json"),
    {ok, Input} = file:read_file(Path ++ "/input.json"),
    {ok, Output} = file:read_file(Path ++ "/output.json"),
    {ok, Rules} = file:read_file(Path ++ "/rules.json"),
    {decode(Aliases), decode(Input), decode(Output), decode(Rules)};
get_conditions(Path, aliases_negative) ->
    {ok, Aliases} = file:read_file(Path ++ "/aliases.json"),
    {ok, Input} = file:read_file(Path ++ "/input.json"),
    {ok, Rules} = file:read_file(Path ++ "/rules.json"),
    {ok, Errors} = file:read_file(Path ++ "/errors.json"),
    {decode(Aliases), decode(Input), decode(Rules), decode(Errors)}.

decode(Json) ->
    try
        jsx:decode(Json)
    catch
        _:_ -> json_parsing_error
    end.
