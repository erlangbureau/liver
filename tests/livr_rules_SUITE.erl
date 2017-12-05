-module(livr_rules_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(LIVR_TEST_PATH, "../../deps/LIVR/test_suite").
-define(EXAMPLE, "liver:validate(~p, ~p) =:= ~p.~n").
-define(TEST_CASES, [

    %% common rules
    required,
    not_empty,
    not_empty_list,
    any_object,

    %% string rules
    string,
    eq,
    one_of,
    max_length,
    min_length,
    length_between,
    length_equal,
    like,

    %% numeric rules
    integer,
    positive_integer,
    decimal,
    positive_decimal,
    max_number,
    min_number,
    number_between,

    %% special rules
%        email,
%        url,
%        iso_date,
%        equal_to_field,
%
    %% meta rules
    nested_object,
    list_of,
    list_of_objects,
    list_of_different_objects,
%        'or',

    %% modifiers (previously - "filter rules")
    trim,
    to_lc,
    to_uc,
    remove,
    leave_only,
    default

]).
-define(RUN(Config),
    Type = ?config(init_type, Config),
    {Rules, Input, Output} = ?config(conditions, Config),
    Expected = expected_output(Type, Output),
    case liver:validate(Rules, Input) =:= Expected of
        true ->
            ok;
        false ->
            io:format(?EXAMPLE, [Rules, Input, Expected]),
            {fail, unsuccessful}
    end
).

all() ->
    [
        {group, positive},
        {group, negative}
    ].

groups() ->
    [
        {positive, [parallel], ?TEST_CASES},
        {negative, [parallel], ?TEST_CASES}
    ].

init_per_group(Name, Config) ->
    [{init_type, Name}|Config].

end_per_group(_Name, Config) ->
    lists:keydelete(init_type, 1, Config).

init_per_testcase(Name, Config) ->
    Type = ?config(init_type, Config),
    case read_conditions(Name, Type) of
        {ok, Conditions} ->
            [{conditions, Conditions}|Config];
        skip ->
            {skipped, no_data}
    end.

end_per_testcase(_Name, Config) ->
    lists:keydelete(conditions, 1, Config).

required(Config) ->
    ?RUN(Config).

not_empty(Config) ->
    ?RUN(Config).

not_empty_list(Config) ->
    ?RUN(Config).

any_object(Config) ->
    ?RUN(Config).

string(Config) ->
    ?RUN(Config).

eq(Config) ->
    ?RUN(Config).

one_of(Config) ->
    ?RUN(Config).

max_length(Config) ->
    ?RUN(Config).

min_length(Config) ->
    ?RUN(Config).

length_between(Config) ->
    ?RUN(Config).

length_equal(Config) ->
    ?RUN(Config).

like(Config) ->
    ?RUN(Config).

integer(Config) ->
    ?RUN(Config).

positive_integer(Config) ->
    ?RUN(Config).

decimal(Config) ->
    ?RUN(Config).

positive_decimal(Config) ->
    ?RUN(Config).

max_number(Config) ->
    ?RUN(Config).

min_number(Config) ->
    ?RUN(Config).

number_between(Config) ->
    ?RUN(Config).

nested_object(Config) ->
    ?RUN(Config).

list_of(Config) ->
    ?RUN(Config).

list_of_objects(Config) ->
    ?RUN(Config).

list_of_different_objects(Config) ->
    ?RUN(Config).

trim(Config) ->
    ?RUN(Config).

to_lc(Config) ->
    ?RUN(Config).

to_uc(Config) ->
    ?RUN(Config).

remove(Config) ->
    ?RUN(Config).

leave_only(Config) ->
    ?RUN(Config).

default(Config) ->
    ?RUN(Config).

%% internal
read_conditions(TestCase, Option) ->
    case case_to_path(TestCase, Option) of
        {ok, Path} ->
            OutputPath = case Option of
                positive -> "output.json";
                negative -> "errors.json"
            end,
            {ok, Rules}     = file:read_file(Path ++ "rules.json"),
            {ok, Input}     = file:read_file(Path ++ "input.json"),
            {ok, Output}    = file:read_file(Path ++ OutputPath),
            {ok, {decode(Rules), decode(Input), decode(Output)}};
        skip ->
            skip
    end.

case_to_path(required, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/01-required/"};
case_to_path(required, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/01-required/"};
case_to_path(not_empty, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/02-not_empty/"};
case_to_path(not_empty, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/02-not_empty/"};
case_to_path(not_empty_list, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/22-not_empty_list/"};
case_to_path(not_empty_list, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/22-not_empty_list/"};
case_to_path(any_object, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/27-any_object/"};
case_to_path(any_object, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/27-any_object/"};
case_to_path(string, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/26-string/"};
case_to_path(string, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/26-string/"};
case_to_path(eq, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/25-eq/"};
case_to_path(eq, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/25-eq/"};
case_to_path(one_of, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/03-one_of/"};
case_to_path(one_of, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/03-one_of/"};
case_to_path(max_length, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/05-max_length/"};
case_to_path(max_length, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/05-max_length/"};
case_to_path(min_length, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/04-min_length/"};
case_to_path(min_length, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/04-min_length/"};
case_to_path(length_between, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/07-length_between/"};
case_to_path(length_between, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/07-length_between/"};
case_to_path(length_equal, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/06-length_equal/"};
case_to_path(length_equal, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/06-length_equal/"};
case_to_path(like, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/08-like/"};
case_to_path(like, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/08-like/"};
case_to_path(integer, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/09-integer/"};
case_to_path(integer, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/09-integer/"};
case_to_path(positive_integer, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/10-positive_integer/"};
case_to_path(positive_integer, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/10-positive_integer/"};
case_to_path(decimal, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/11-decimal/"};
case_to_path(decimal, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/11-decimal/"};
case_to_path(positive_decimal, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/12-positive_decimal/"};
case_to_path(positive_decimal, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/12-positive_decimal/"};
case_to_path(max_number, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/13-max_number/"};
case_to_path(max_number, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/13-max_number/"};
case_to_path(min_number, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/14-min_number/"};
case_to_path(min_number, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/14-min_number/"};
case_to_path(number_between, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/15-number_between/"};
case_to_path(number_between, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/15-number_beetween/"};
case_to_path(nested_object, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/18-nested_object/"};
case_to_path(nested_object, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/18-nested_object/"};
case_to_path(list_of, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/19-list_of/"};
case_to_path(list_of, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/19-list_of/"};
case_to_path(list_of_objects, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/20-list_of_objects/"};
case_to_path(list_of_objects, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/20-list_of_objects/"};
case_to_path(list_of_different_objects, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/21-list_of_different_objects/"};
case_to_path(list_of_different_objects, negative) ->
    {ok, ?LIVR_TEST_PATH ++ "/negative/21-list_of_different_objects/"};

case_to_path(trim, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/30-trim/"};
case_to_path(to_lc, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/31-to_lc/"};
case_to_path(to_uc, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/32-to_uc/"};
case_to_path(remove, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/33-remove/"};
case_to_path(leave_only, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/34-leave_only/"};
case_to_path(default, positive) ->
    {ok, ?LIVR_TEST_PATH ++ "/positive/35-default/"};
case_to_path(_, _) ->
    skip.

expected_output(positive, Output) ->
    {ok, Output};
expected_output(negative, Output) ->
    {error, Output}.

decode(Json) ->
    try
        jsx:decode(Json, [])
    catch
        _:_ -> json_parsing_error
    end.
