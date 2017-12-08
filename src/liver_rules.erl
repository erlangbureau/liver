-module(liver_rules).

%% API
-export([normalize/2]).
-export([apply/3]).
-export([has_required/1]).
-export([apply_required/2]).

%% API
normalize(Rules, Data) ->
    Result = normalize(Rules, Data, []),
    lists:reverse(Result).

apply(Rules, Value, Opts) ->
    ApplyFun = fun(Rule, RuleArgs, Acc) ->
        apply(Rule, RuleArgs, Acc, Opts)
    end,
    liver_maps:fold(ApplyFun, {ok, Value}, Rules).

has_required([{R, _A}|Rules]) ->
    case is_required(R) of
        true -> true;
        false -> has_required(Rules)
    end;
has_required([]) ->
    false.

apply_required(Rules, Opts) ->
    Rules2 = [Rule || {R, _Args} = Rule <- Rules, is_required(R)],
    ApplyFun = fun(Rule, RuleArgs, Acc) ->
        apply(Rule, RuleArgs, Acc, Opts)
    end,
    liver_maps:fold(ApplyFun, {ok, undefined}, Rules2).


%% internal
normalize([{K, V} = Rule|Rules], Data, Acc) when is_atom(K) ->
    case K of
        equal_to_field ->
            Name = case V of
                [FieldName|_]   -> FieldName;
                FieldName       -> FieldName
            end,
            FieldValue = liver_maps:get(Name, Data, undefined),
            normalize(Rules, Data, [{equal_to_field, FieldValue}|Acc]);
        _ ->
            normalize(Rules, Data, [Rule|Acc])
    end;
normalize(Rules, Data, Acc) when is_map(Rules) ->
    Rules2 = maps:to_list(Rules),
    normalize(Rules2, Data, Acc);
normalize([{K, V}|Rules], Data, Acc) when is_binary(K) ->
    K2 = binary_to_atom(K, utf8),
    normalize([{K2, V}|Rules], Data, Acc);
normalize(K, Data, Acc) when is_atom(K) ->
    normalize([{K, []}], Data, Acc);
normalize(Rule, Data, Acc) when is_binary(Rule) ->
    K2 = binary_to_atom(Rule, utf8),
    normalize([{K2, []}], Data, Acc);
normalize([Rule|Rules], Data, Acc) when is_map(Rule) ->
    Rules2 = maps:to_list(Rule),
    Acc2 = normalize(Rules2, Data, Acc),
    normalize(Rules, Data, Acc2);
normalize([Rules|Rules2], Data, Acc) when is_list(Rules) ->
    Acc2 = normalize(Rules, Data, Acc),
    normalize(Rules2, Data, Acc2);
normalize([K|Rules], Data, Acc) when is_atom(K) ->
    normalize(Rules, Data, [{K, []}|Acc]);
normalize([K|Rules], Data, Acc) when is_binary(K) ->
    K2 = binary_to_atom(K, utf8),
    normalize(Rules, Data, [{K2, []}|Acc]);
normalize([], _Data, Acc) ->
    Acc.

is_required(required)       -> true;
is_required(not_empty_list) -> true;
is_required(default)        -> true;
is_required(_)              -> false.

apply(Rule, Args, {ok, Value}, Opts) ->
    Module = liver:which(Rule),
    try Module:Rule(Args, Value, Opts)
    catch
        error:undef ->
            {error, {unimplemented_rule, Rule}};
        _Type:_Reason ->
            {error, format_error}
    end;
apply(_Rule, _Args, {error, _} = Err, _Opts) ->
    Err.
