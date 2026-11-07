-module(liver_ct_cases).

-export([run/2]).

-include_lib("common_test/include/ct.hrl").

run(Case, Config) ->
    Kind = ?config(init_type, Config),
    Form = ?config(data_form, Config),
    Suite = ?config(cases_suite, Config),
    DefaultOpts = ?config(validate_opts, Config),
    CaseData = load_case(Suite, Form, Kind, Case),
    case expectation(Kind) of
        schema_error -> run_schema_error(Suite, Form, Case, CaseData, DefaultOpts);
        Expectation -> run(Suite, Form, Kind, Case, Expectation, CaseData, DefaultOpts)
    end.

%% schema_errors: liver:check_schema/2 rejects the schema.
run_schema_error(Suite, Form, Case, CaseData, DefaultOpts) ->
    {Rules, Opts} = case Form of
        maps ->
            {maps:get(rules, CaseData),
             maps:merge(DefaultOpts, maps:get(opts, CaseData, #{}))};
        proplists ->
            {ok_get(rules, CaseData),
             maps:merge(DefaultOpts, to_map(ok_get_default(opts, CaseData, [])))}
    end,
    case liver:check_schema(Rules, Opts) of
        {error, _Reason} ->
            ok;
        ok ->
            ct:pal("suite=~p form=~p case=~p~nliver:check_schema(~p, ~p).~nGot: ok~n",
                   [Suite, Form, Case, Rules, Opts]),
            ct:fail({schema_accepted, Suite, Case, Form})
    end.

run(Suite, Form, Kind, Case, Expectation, CaseData, DefaultOpts) ->
    {Rules, Input, Expected, Opts} = unpack(Expectation, Form, CaseData, DefaultOpts),
    case liver:check_schema(Rules, Opts) of
        ok ->
            ok;
        SchemaError ->
            ct:pal("suite=~p form=~p kind=~p case=~p~nliver:check_schema(~p, ~p).~nGot: ~p~n",
                   [Suite, Form, Kind, Case, Rules, Opts, SchemaError]),
            ct:fail({schema_rejected, Suite, Case, Kind, Form})
    end,
    ok = fix_clock(now(Form, CaseData)),
    case liver:validate(Rules, Input, Opts) of
        Expected ->
            ok;
        Actual ->
            ct:pal("suite=~p form=~p kind=~p case=~p~n"
                   "liver:validate(~p, ~p, ~p).~nGot: ~p~nExpected: ~p~n",
                   [Suite, Form, Kind, Case, Rules, Input, Opts, Actual, Expected]),
            ct:fail({unsuccessful, Suite, Case, Kind, Form})
    end.

%% internal

%% options.json of the LIVR 2.5 cases: the current time for today,
%% yesterday and tomorrow, see liver_clock.
now(maps, CaseData) -> maps:get(now, CaseData, undefined);
now(proplists, CaseData) -> ok_get_default(now, CaseData, undefined).

fix_clock(undefined) ->
    ok;
fix_clock(Now) ->
    Ms = calendar:rfc3339_to_system_time(binary_to_list(Now), [{unit, millisecond}]),
    liver_clock:set_time(Ms).

expectation(schema_errors) -> schema_error;
expectation(aliases_positive) -> positive;
expectation(aliases_negative) -> negative;
expectation(Kind) -> Kind.

unpack(positive, maps, #{rules := Rules, input := Input, output := Output} = Data, DefaultOpts) ->
    {Rules, Input, {ok, Output}, maps:merge(DefaultOpts, maps:get(opts, Data, #{}))};
unpack(negative, maps, #{rules := Rules, input := Input, errors := Errors} = Data, DefaultOpts) ->
    {Rules, Input, {error, Errors}, maps:merge(DefaultOpts, maps:get(opts, Data, #{}))};
unpack(positive, proplists, Case, DefaultOpts) when is_list(Case) ->
    Opts = maps:merge(DefaultOpts, to_map(ok_get_default(opts, Case, []))),
    {ok_get(rules, Case), ok_get(input, Case), {ok, ok_get(output, Case)}, Opts};
unpack(negative, proplists, Case, DefaultOpts) when is_list(Case) ->
    Opts = maps:merge(DefaultOpts, to_map(ok_get_default(opts, Case, []))),
    {ok_get(rules, Case), ok_get(input, Case), {error, ok_get(errors, Case)}, Opts}.

to_map(Map) when is_map(Map) ->
    Map;
to_map(List) when is_list(List) ->
    maps:from_list(List).

ok_get(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false -> error({missing_case_key, Key, List})
    end.

ok_get_default(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false -> Default
    end.

load_case(Suite, Form, Kind, Case) ->
    Path = case_path(Suite, Form, Kind, Case),
    case file:consult(Path) of
        {ok, [Data]} ->
            Data;
        {ok, Other} ->
            ct:fail({bad_case_file, Path, Other});
        {error, Reason} ->
            ct:fail({case_read_error, Path, Reason})
    end.

case_path(Suite, Form, Kind, Case) ->
    KindDir = filename:join([cases_root(Suite), "cases", atom_to_list(Suite),
                             atom_to_list(Form), atom_to_list(Kind)]),
    {ok, Files} = file:list_dir(KindDir),
    Suffix = "-" ++ atom_to_list(Case) ++ ".config",
    case [F || F <- Files, lists:suffix(Suffix, F)] of
        [File] ->
            filename:join(KindDir, File);
        [] when Suite =:= livr, Case =:= number_between ->
            Alt = [F || F <- Files, lists:suffix("-number_beetween.config", F)],
            case Alt of
                [File] -> filename:join(KindDir, File);
                _ -> ct:fail({case_file_not_found, Suite, Form, Kind, Case, KindDir})
            end;
        [] ->
            ct:fail({case_file_not_found, Suite, Form, Kind, Case, KindDir});
        Many ->
            ct:fail({ambiguous_case_file, Suite, Form, Kind, Case, Many})
    end.

cases_root(Suite) ->
    Rel = filename:join(["cases", atom_to_list(Suite)]),
    case filelib:is_dir(Rel) of
        true ->
            ".";
        false ->
            UnderTests = filename:join(["tests", Rel]),
            case filelib:is_dir(UnderTests) of
                true ->
                    "tests";
                false ->
                    Beam = code:which(?MODULE),
                    SrcDir = filename:dirname(Beam),
                    case filelib:is_dir(filename:join(SrcDir, Rel)) of
                        true -> SrcDir;
                        false -> "."
                    end
            end
    end.
