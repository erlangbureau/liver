%% Import LIVR JSON test_suite into local Erlang .config cases.
%%
%% Decode path (important for honesty of both fixture sets):
%%   1) jsx:decode/1  -> proplists (JSON key order preserved)
%%   2) convert rules on that proplist structure
%%   3) apply_known_patches/3 for documented LIVR/JSON mismatches
%%   4) write tests/cases/livr/proplists/{positive,negative}/*.config
%%   5) convert the same terms to maps and write
%%      tests/cases/livr/maps/{positive,negative}/*.config
%%
%% Run via: make livr-spec-import
-module(livr_import).

-export([run/3]).

-define(LIVR_RULES, #{
    <<"required">> => true,
    <<"not_empty">> => true,
    <<"not_empty_list">> => true,
    <<"any_object">> => true,
    <<"string">> => true,
    <<"eq">> => true,
    <<"one_of">> => true,
    <<"max_length">> => true,
    <<"min_length">> => true,
    <<"length_between">> => true,
    <<"length_equal">> => true,
    <<"like">> => true,
    <<"integer">> => true,
    <<"positive_integer">> => true,
    <<"decimal">> => true,
    <<"positive_decimal">> => true,
    <<"max_number">> => true,
    <<"min_number">> => true,
    <<"number_between">> => true,
    <<"email">> => true,
    <<"url">> => true,
    <<"iso_date">> => true,
    <<"equal_to_field">> => true,
    <<"nested_object">> => true,
    <<"variable_object">> => true,
    <<"list_of">> => true,
    <<"list_of_objects">> => true,
    <<"list_of_different_objects">> => true,
    <<"or">> => true,
    <<"trim">> => true,
    <<"to_lc">> => true,
    <<"to_uc">> => true,
    <<"remove">> => true,
    <<"leave_only">> => true,
    <<"default">> => true
}).

%% API
run(Source, Sha, Out) when is_list(Source), is_list(Sha), is_list(Out) ->
    ok = ensure_tree(Out),
    {PosNames, PosEntries} = import_kind(Source, Out, Sha, "positive"),
    {NegNames, NegEntries} = import_kind(Source, Out, Sha, "negative"),
    ok = write_manifest(Out, Sha, PosEntries ++ NegEntries),
    ok = write_cases_hrl(Out, PosNames, NegNames),
    ok = write_runners_hrl(Out, unique(PosNames ++ NegNames)),
    io:format("Imported ~p cases (maps+proplists) from LIVR@~s into ~s~n",
              [length(PosEntries) + length(NegEntries), string:slice(Sha, 0, 8), Out]),
    ok.

%% internal
ensure_tree(Out) ->
    lists:foreach(fun(Dir) -> ok = ensure_dir(Dir) end, [
        Out,
        filename:join(Out, "maps"),
        filename:join(Out, "proplists"),
        filename:join([Out, "maps", "positive"]),
        filename:join([Out, "maps", "negative"]),
        filename:join([Out, "proplists", "positive"]),
        filename:join([Out, "proplists", "negative"])
    ]).

import_kind(Source, Out, Sha, Kind) ->
    Root = filename:join([Source, "test_suite", Kind]),
    {ok, Entries0} = file:list_dir(Root),
    Dirs = lists:sort([E || E <- Entries0,
                            filelib:is_dir(filename:join(Root, E))]),
    lists:foldl(fun(Dir, {NamesAcc, EntriesAcc}) ->
        Atom = case_atom(Dir),
        %% 1) Decode as proplist first to keep JSON key order.
        CasePL0 = build_case_proplist(Root, Dir, Kind, Atom),
        {CasePL, Patched} = apply_known_patches(Atom, Kind, CasePL0),
        PLFile = filename:join([Out, "proplists", Kind, Dir ++ ".config"]),
        ok = write_case(PLFile, proplists, Kind, Dir, Atom, Sha, CasePL, Patched),
        %% 2) Derive maps from the same proplist terms.
        CaseMap = proplist_case_to_map(CasePL),
        MapFile = filename:join([Out, "maps", Kind, Dir ++ ".config"]),
        ok = write_case(MapFile, maps, Kind, Dir, Atom, Sha, CaseMap, Patched),
        {[Atom | NamesAcc], [Kind ++ "/" ++ Dir | EntriesAcc]}
    end, {[], []}, Dirs).

build_case_proplist(Root, Dir, Kind, Atom) ->
    CaseDir = filename:join(Root, Dir),
    Rules = convert_schema_pl(decode_proplist(filename:join(CaseDir, "rules.json"))),
    Input = decode_proplist(filename:join(CaseDir, "input.json")),
    case Kind of
        "positive" ->
            Output0 = decode_proplist(filename:join(CaseDir, "output.json")),
            Output = case Atom of
                iso_date -> convert_iso_date_output(Output0);
                _ -> Output0
            end,
            [{name, Atom}, {rules, Rules}, {input, Input}, {output, Output}];
        "negative" ->
            Errors = decode_proplist(filename:join(CaseDir, "errors.json")),
            [{name, Atom}, {rules, Rules}, {input, Input}, {errors, Errors}]
    end.

%% Known LIVR JSON issue: objects are unordered, Erlang proplists are not.
%% liver emits nested error fields in schema order; some errors.json files
%% use a different key order. Patch those fixtures so strict =:= tests fail
%% only on real mismatches, not on JSON key order.
apply_known_patches('or', "negative", Case) ->
    {set_key(errors, Case, patch_or_negative_errors(ok_get(errors, Case))),
     [or_negative_nested_error_key_order]};
apply_known_patches(_Atom, _Kind, Case) ->
    {Case, []}.

%% negative/29-or: first products[] error object is
%%   {"name": "REQUIRED", "product_type": "NOT_ALLOWED_VALUE"}
%% in JSON, but liver returns product_type then name (schema order of the
%% matching nested_object / or branch).
patch_or_negative_errors(Errors) ->
    Products = ok_get(<<"products">>, Errors),
    [First | Rest] = Products,
    First2 = [
        {<<"product_type">>, ok_get(<<"product_type">>, First)},
        {<<"name">>, ok_get(<<"name">>, First)}
    ],
    set_key(<<"products">>, Errors, [First2 | Rest]).

ok_get(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false -> error({missing_key, Key})
    end.

set_key(Key, List, Value) ->
    lists:keystore(Key, 1, List, {Key, Value}).

decode_proplist(Path) ->
    {ok, Bin} = file:read_file(Path),
    try
        %% No return_maps: objects become proplists, key order preserved.
        jsx:decode(Bin, [])
    catch
        _:Reason ->
            error({json_decode_failed, Path, Reason})
    end.

%% ---- rule conversion on proplists (order-preserving) ----

convert_schema_pl([{}]) ->
    [{}];
convert_schema_pl(List) when is_list(List) ->
    case is_object_proplist(List) of
        true ->
            [{K, convert_rule_pl(V)} || {K, V} <- List];
        false ->
            error({expected_schema_object, List})
    end.

convert_rule_pl(Value) when is_binary(Value) ->
    case is_rule_name(Value) of
        true -> binary_to_atom(Value, utf8);
        false -> Value
    end;
convert_rule_pl([{}]) ->
    [{}];
convert_rule_pl(List) when is_list(List) ->
    case is_object_proplist(List) of
        true ->
            %% Rule object: [{"max_length", 5}] / [{"nested_object", [...]}]
            lists:map(fun({K, V}) ->
                case is_rule_name(K) of
                    true ->
                        Rule = binary_to_atom(K, utf8),
                        {Rule, convert_rule_args_pl(Rule, V)};
                    false ->
                        {K, convert_data_pl(V)}
                end
            end, List);
        false ->
            [convert_rule_pl(V) || V <- List]
    end;
convert_rule_pl(Other) ->
    Other.

convert_rule_args_pl(nested_object, Args) ->
    convert_schema_pl(Args);
convert_rule_args_pl(list_of_objects, Args) ->
    convert_schema_pl(Args);
convert_rule_args_pl(list_of, Args) ->
    convert_rule_pl(Args);
convert_rule_args_pl('or', Args) when is_list(Args) ->
    [convert_rule_pl(V) || V <- Args];
convert_rule_args_pl(variable_object, [Discriminator, Variants]) ->
    [Discriminator, convert_variants_pl(Variants)];
convert_rule_args_pl(list_of_different_objects, [Discriminator, Variants]) ->
    [Discriminator, convert_variants_pl(Variants)];
convert_rule_args_pl(_Rule, Args) ->
    convert_data_pl(Args).

convert_variants_pl([{}]) ->
    [{}];
convert_variants_pl(List) when is_list(List) ->
    [{Name, convert_schema_pl(Schema)} || {Name, Schema} <- List].

convert_data_pl([{}]) ->
    [{}];
convert_data_pl(List) when is_list(List) ->
    case is_object_proplist(List) of
        true -> [{K, convert_data_pl(V)} || {K, V} <- List];
        false -> [convert_data_pl(V) || V <- List]
    end;
convert_data_pl(Other) ->
    Other.

%% ---- proplist case -> map case ----

proplist_case_to_map(CasePL) ->
    maps:from_list([{K, pl_to_map(V)} || {K, V} <- CasePL]).

pl_to_map([{}]) ->
    #{};
pl_to_map(List) when is_list(List) ->
    case is_object_proplist(List) of
        true ->
            maps:from_list([{K, pl_to_map(V)} || {K, V} <- List]);
        false ->
            [pl_to_map(V) || V <- List]
    end;
pl_to_map(Other) ->
    Other.

%% ---- iso_date adaptation (works on proplist/list/binary) ----

convert_iso_date_output([{}]) ->
    [{}];
convert_iso_date_output(List) when is_list(List) ->
    case is_object_proplist(List) of
        true ->
            [{K, convert_iso_date_output(V)} || {K, V} <- List];
        false ->
            [convert_iso_date_output(V) || V <- List]
    end;
convert_iso_date_output(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = Bin) ->
    try
        {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}
    catch
        _:_ -> Bin
    end;
convert_iso_date_output(Other) ->
    Other.

is_rule_name(Name) when is_binary(Name) ->
    maps:is_key(Name, ?LIVR_RULES);
is_rule_name(_) ->
    false.

is_object_proplist([{}]) ->
    true;
is_object_proplist([]) ->
    false;
is_object_proplist(List) when is_list(List) ->
    lists:all(fun
        ({_K, _V}) -> true;
        (_) -> false
    end, List);
is_object_proplist(_) ->
    false.

case_atom(Dir) ->
    case re:run(Dir, "^\\d+-(.+)$", [{capture, all_but_first, list}]) of
        {match, [Name0]} ->
            Name = case Name0 of
                "number_beetween" -> "number_between";
                _ -> Name0
            end,
            list_to_atom(Name);
        nomatch ->
            error({bad_case_dir, Dir})
    end.

write_case(Path, Form, Kind, Dir, Atom, Sha, CaseTerm, Patches) ->
    PatchNote = case Patches of
        [] ->
            [];
        _ ->
            io_lib:format(
                "%% Known issue (patched on import): ~p~n"
                "%% JSON objects are unordered; liver proplist errors follow schema order.~n",
                [Patches])
    end,
    Header = io_lib:format(
        "%% LIVR ~s/~s (case: ~p, form: ~p)~n"
        "%% Source: https://github.com/koorchik/LIVR/tree/~s/test_suite/~s/~s~n"
        "%% Imported for liver; iso_date outputs use Erlang {Y,M,D} dates.~n"
        "%% Proplist fixtures preserve JSON key order from jsx:decode/1.~n"
        "~s~n",
        [Kind, Dir, Atom, Form, Sha, Kind, Dir, PatchNote]),
    Body = [format_term(CaseTerm, 0), ".\n"],
    ok = file:write_file(Path, unicode:characters_to_binary([Header, Body])).

format_term(null, _I) ->
    "null";
format_term(true, _I) ->
    "true";
format_term(false, _I) ->
    "false";
format_term(Atom, _I) when is_atom(Atom) ->
    io_lib:write_atom(Atom);
format_term(Int, _I) when is_integer(Int) ->
    integer_to_list(Int);
format_term(Float, _I) when is_float(Float) ->
    float_to_list(Float, [short]);
format_term(Bin, _I) when is_binary(Bin) ->
    format_binary(Bin);
format_term({Y, M, D}, _I)
  when is_integer(Y), is_integer(M), is_integer(D) ->
    [${, integer_to_list(Y), ", ", integer_to_list(M), ", ",
     integer_to_list(D), $}];
format_term(Map, I) when is_map(Map) ->
    case maps:size(Map) of
        0 ->
            "#{}";
        _ ->
            Pad1 = lists:duplicate((I + 1) * 4, $\s),
            Pad0 = lists:duplicate(I * 4, $\s),
            Items = [[Pad1, format_term(K, I + 1), " => ", format_term(V, I + 1)]
                     || {K, V} <- maps:to_list(Map)],
            ["#{\n", lists:join(",\n", Items), "\n", Pad0, "}"]
    end;
format_term([{}], _I) ->
    "[{}]";
format_term([], _I) ->
    "[]";
format_term(List, I) when is_list(List) ->
    case is_object_proplist(List) of
        true ->
            Pad1 = lists:duplicate((I + 1) * 4, $\s),
            Pad0 = lists:duplicate(I * 4, $\s),
            Items = [[Pad1, ${, format_term(K, I + 1), ", ",
                      format_term(V, I + 1), $}]
                     || {K, V} <- List],
            ["[\n", lists:join(",\n", Items), "\n", Pad0, "]"];
        false ->
            case lists:all(fun(V) ->
                                is_binary(V) orelse is_number(V) orelse is_atom(V)
                                    orelse V =:= null orelse is_boolean(V)
                            end, List) andalso length(List) =< 8 of
                true ->
                    ["[", lists:join(", ", [format_term(V, I) || V <- List]), "]"];
                false ->
                    Pad1 = lists:duplicate((I + 1) * 4, $\s),
                    Pad0 = lists:duplicate(I * 4, $\s),
                    Items = [[Pad1, format_term(V, I + 1)] || V <- List],
                    ["[\n", lists:join(",\n", Items), "\n", Pad0, "]"]
            end
    end.

format_binary(<<>>) ->
    "<<>>";
format_binary(Bin) ->
    case unicode:characters_to_list(Bin) of
        Chars when is_list(Chars) ->
            case is_mostly_printable(Chars) of
                true ->
                    Esc = escape_erl_string(Chars),
                    case lists:all(fun(C) -> C =< 127 end, Chars) of
                        true -> ["<<\"", Esc, "\">>"];
                        false -> ["<<\"", Esc, "\"/utf8>>"]
                    end;
                false ->
                    io_lib:format("~w", [Bin])
            end;
        _ ->
            io_lib:format("~w", [Bin])
    end.

is_mostly_printable(Chars) ->
    lists:all(fun(C) ->
        (C >= 32 andalso C =/= 127) orelse lists:member(C, [$\n, $\t, $\r])
    end, Chars).

escape_erl_string(Chars) ->
    [case C of
         $\\ -> "\\\\";
         $" -> "\\\"";
         $\n -> "\\n";
         $\t -> "\\t";
         $\r -> "\\r";
         _ -> C
     end || C <- Chars].

write_manifest(Out, Sha, Entries) ->
    Lines = [
        "sha " ++ Sha, $\n,
        "url https://github.com/koorchik/LIVR", $\n,
        "suites positive negative", $\n,
        "forms maps proplists", $\n, $\n,
        "# cases", $\n,
        [[E, $\n] || E <- lists:reverse(Entries)],
        $\n
    ],
    ok = file:write_file(filename:join(Out, "MANIFEST"), Lines).

write_cases_hrl(Out, PosNames, NegNames) ->
    Content = [
        "%% Generated by livr_import - do not edit by hand.\n",
        "-define(LIVR_POSITIVE_CASES, [\n",
        format_atom_list(lists:reverse(PosNames)),
        "]).\n\n",
        "-define(LIVR_NEGATIVE_CASES, [\n",
        format_atom_list(lists:reverse(NegNames)),
        "]).\n"
    ],
    ok = file:write_file(filename:join(Out, "cases.hrl"), Content).

write_runners_hrl(Out, Names) ->
    Clauses = [[format_atom(A), "(Config) -> liver_ct_cases:run(",
                format_atom(A), ", Config).\n"] || A <- Names],
    Content = ["%% Generated by livr_import - do not edit by hand.\n", Clauses],
    ok = file:write_file(filename:join(Out, "runners.hrl"), Content).

format_atom_list([]) ->
    "";
format_atom_list([A]) ->
    ["    ", format_atom(A), "\n"];
format_atom_list([A | Rest]) ->
    ["    ", format_atom(A), ",\n", format_atom_list(Rest)].

format_atom(A) ->
    io_lib:write_atom(A).

unique(List) ->
    lists:reverse(lists:foldl(fun(X, Acc) ->
        case lists:member(X, Acc) of
            true -> Acc;
            false -> [X | Acc]
        end
    end, [], List)).

ensure_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
            case file:make_dir(Dir) of
                ok -> ok;
                {error, eexist} -> ok;
                Error -> Error
            end
    end.
