-module(liver_ct_cases).

-export([run/2]).

-include_lib("common_test/include/ct.hrl").

run(Case, Config) ->
    Kind = ?config(init_type, Config),
    Form = ?config(data_form, Config),
    CaseData = load_case(Form, Kind, Case),
    {Rules, Input, Expected} = unpack(Kind, Form, CaseData),
    case liver:validate(Rules, Input) of
        Expected ->
            ok;
        Actual ->
            ct:pal("form=~p kind=~p case=~p~nliver:validate(~p, ~p).~nGot: ~p~nExpected: ~p~n",
                   [Form, Kind, Case, Rules, Input, Actual, Expected]),
            ct:fail({unsuccessful, Case, Kind, Form})
    end.

%% internal
unpack(positive, maps, #{rules := Rules, input := Input, output := Output}) ->
    {Rules, Input, {ok, Output}};
unpack(negative, maps, #{rules := Rules, input := Input, errors := Errors}) ->
    {Rules, Input, {error, Errors}};
unpack(positive, proplists, Case) when is_list(Case) ->
    {ok_get(rules, Case), ok_get(input, Case), {ok, ok_get(output, Case)}};
unpack(negative, proplists, Case) when is_list(Case) ->
    {ok_get(rules, Case), ok_get(input, Case), {error, ok_get(errors, Case)}}.

ok_get(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false -> error({missing_case_key, Key, List})
    end.

load_case(Form, Kind, Case) ->
    Path = case_path(Form, Kind, Case),
    case file:consult(Path) of
        {ok, [Data]} ->
            Data;
        {ok, Other} ->
            ct:fail({bad_case_file, Path, Other});
        {error, Reason} ->
            ct:fail({case_read_error, Path, Reason})
    end.

case_path(Form, Kind, Case) ->
    KindDir = filename:join([cases_root(), "cases/livr",
                             atom_to_list(Form), atom_to_list(Kind)]),
    {ok, Files} = file:list_dir(KindDir),
    Suffix = "-" ++ atom_to_list(Case) ++ ".config",
    case [F || F <- Files, lists:suffix(Suffix, F)] of
        [File] ->
            filename:join(KindDir, File);
        [] when Case =:= number_between ->
            Alt = [F || F <- Files, lists:suffix("-number_beetween.config", F)],
            case Alt of
                [File] -> filename:join(KindDir, File);
                _ -> ct:fail({case_file_not_found, Form, Kind, Case, KindDir})
            end;
        [] ->
            ct:fail({case_file_not_found, Form, Kind, Case, KindDir});
        Many ->
            ct:fail({ambiguous_case_file, Form, Kind, Case, Many})
    end.

cases_root() ->
    case filelib:is_dir("cases/livr") of
        true ->
            ".";
        false ->
            case filelib:is_dir("tests/cases/livr") of
                true ->
                    "tests";
                false ->
                    Beam = code:which(?MODULE),
                    SrcDir = filename:dirname(Beam),
                    case filelib:is_dir(filename:join(SrcDir, "cases/livr")) of
                        true -> SrcDir;
                        false -> "."
                    end
            end
    end.
