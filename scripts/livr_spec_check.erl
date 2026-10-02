%% Compare local LIVR MANIFEST against upstream test_suite directories
%% and ensure both maps/ and proplists/ fixture trees exist.
%%
%% Run via: make livr-spec-check
-module(livr_spec_check).

-export([run/2]).

run(Source, ManifestPath) when is_list(Source), is_list(ManifestPath) ->
    CasesRoot = filename:dirname(ManifestPath),
    {Sha, Local} = read_manifest(ManifestPath),
    Remote = upstream_cases(Source),
    Missing = lists:sort(sets:to_list(sets:subtract(Remote, Local))),
    Extra = lists:sort(sets:to_list(sets:subtract(Local, Remote))),
    FormGaps = missing_form_files(CasesRoot, Local),
    io:format("Local MANIFEST sha: ~s~n", [Sha]),
    io:format("Local cases: ~p; upstream positive+negative: ~p~n",
              [sets:size(Local), sets:size(Remote)]),
    case {Missing, Extra, FormGaps} of
        {[], [], []} ->
            io:format("OK: local LIVR cases match upstream; maps+proplists present.~n"),
            ok;
        _ ->
            [io:format("NEW upstream cases not present locally:~n") || Missing =/= []],
            [io:format("  + ~s~n", [C]) || C <- Missing],
            [io:format("Local cases missing upstream (removed or renamed):~n") || Extra =/= []],
            [io:format("  - ~s~n", [C]) || C <- Extra],
            [io:format("Missing local form fixtures:~n") || FormGaps =/= []],
            [io:format("  ! ~s~n", [C]) || C <- FormGaps],
            io:format("Run: make livr-spec-import~n"),
            error(livr_spec_drift)
    end.

%% internal
read_manifest(Path) ->
    {ok, Bin} = file:read_file(Path),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    lists:foldl(fun(Line0, {Sha, Cases}) ->
        Line = string:trim(Line0),
        case Line of
            <<>> -> {Sha, Cases};
            <<"#", _/binary>> -> {Sha, Cases};
            <<"sha ", Rest/binary>> -> {binary_to_list(Rest), Cases};
            <<"url ", _/binary>> -> {Sha, Cases};
            <<"suites ", _/binary>> -> {Sha, Cases};
            <<"forms ", _/binary>> -> {Sha, Cases};
            _ -> {Sha, sets:add_element(binary_to_list(Line), Cases)}
        end
    end, {"(unknown)", sets:new()}, Lines).

upstream_cases(Source) ->
    lists:foldl(fun(Kind, Acc) ->
        Root = filename:join([Source, "test_suite", Kind]),
        case file:list_dir(Root) of
            {ok, Entries} ->
                lists:foldl(fun(E, Acc2) ->
                    case filelib:is_dir(filename:join(Root, E)) of
                        true -> sets:add_element(Kind ++ "/" ++ E, Acc2);
                        false -> Acc2
                    end
                end, Acc, Entries);
            {error, Reason} ->
                error({missing_suite_dir, Root, Reason})
        end
    end, sets:new(), ["positive", "negative"]).

missing_form_files(CasesRoot, Local) ->
    lists:sort(lists:foldl(fun(Case, Acc) ->
        lists:foldl(fun(Form, Acc2) ->
            Path = filename:join([CasesRoot, Form, Case ++ ".config"]),
            case filelib:is_regular(Path) of
                true -> Acc2;
                false -> [Form ++ "/" ++ Case ++ ".config" | Acc2]
            end
        end, Acc, ["maps", "proplists"])
    end, [], sets:to_list(Local))).
