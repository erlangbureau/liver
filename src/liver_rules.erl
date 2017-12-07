-module(liver_rules).

%% API
-export([normalize/1, normalize/2]).
-export([apply/3]).
-export([has_required/1]).
-export([apply_required/2]).

%% API
normalize(Rules) ->
    normalize(Rules, []).

normalize(Rules, Data) ->
    Result = normilize(Rules, Data, []),
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
normilize([{K, V} = Rule|Rules], Data, Acc) when is_atom(K) ->
    case K of
        equal_to_field ->
            Name = case V of
                [FieldName|_]   -> FieldName;
                FieldName       -> FieldName
            end,
            FieldValue = liver_maps:get(Name, Data, undefined),
            normilize(Rules, Data, [{equal_to_field, FieldValue}|Acc]);
        _ ->
            normilize(Rules, Data, [Rule|Acc])
    end;
normilize(Rules, Data, Acc) when is_map(Rules) ->
    Rules2 = maps:to_list(Rules),
    normilize(Rules2, Data, Acc);
normilize([{K, V}|Rules], Data, Acc) when is_binary(K) ->
    K2 = binary_to_atom(K, utf8),
    normilize([{K2, V}|Rules], Data, Acc);
normilize(K, Data, Acc) when is_atom(K) ->
    normilize([{K, []}], Data, Acc);
normilize(Rule, Data, Acc) when is_binary(Rule) ->
    K2 = binary_to_atom(Rule, utf8),
    normilize([{K2, []}], Data, Acc);
normilize([Rule|Rules], Data, Acc) when is_map(Rule) ->
    Rules2 = maps:to_list(Rule),
    Acc2 = normilize(Rules2, Data, Acc),
    normilize(Rules, Data, Acc2);
normilize([Rules|Rules2], Data, Acc) when is_list(Rules) ->
    Acc2 = normilize(Rules, Data, Acc),
    normilize(Rules2, Data, Acc2);
normilize([K|Rules], Data, Acc) when is_atom(K) ->
    normilize(Rules, Data, [{K, []}|Acc]);
normilize([K|Rules], Data, Acc) when is_binary(K) ->
    K2 = binary_to_atom(K, utf8),
    normilize(Rules, Data, [{K2, []}|Acc]);
normilize([], _Data, Acc) ->
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
