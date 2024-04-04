-module(liver_rules).

%% API
-export([normalize/2]).
-export([has_required/1]).
-export([execute/2, execute/3]).

-include("liver_rules.hrl").

%% API
normalize(Rules, Data) ->
    Result = normalize(Rules, Data, []),
    lists:reverse(Result).

has_required([{R, _A}|Rules]) ->
    case is_required(R) of
        true -> true;
        false -> has_required(Rules)
    end;
has_required([]) ->
    false.

execute(Rules, Opts) ->
    Rules2 = [Rule || {R, _Args} = Rule <- Rules, is_required(R)],
    execute(Rules2, ?MISSED_FIELD_VALUE, Opts).

execute([{Rule, Args}|Rules], Value, Opts) ->
    Module = liver:which(Rule),
    try Module:Rule(Args, Value, Opts) of
        {ok, Value2}    -> execute(Rules, Value2, Opts);
        Error           -> Error
    catch
        error:undef ->
            {error, {unimplemented_rule, Rule}};
        _Type:_Reason ->
            {error, format_error}
    end;
execute([], Value, _Opts) ->
    {ok, Value}.

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
normalize({K, V}, Data, Acc) when is_atom(K) ->
    normalize([{K, V}], Data, Acc);
normalize({K, V}, Data, Acc) when is_binary(K) ->
    K2 = binary_to_atom(K, utf8),
    normalize([{K2, V}], Data, Acc);
normalize([Rule|Rules], Data, Acc) when is_map(Rule) ->
    Rules2 = maps:to_list(Rule),
    Acc2 = normalize(Rules2, Data, Acc),
    normalize(Rules, Data, Acc2);
normalize([Rules|Rules2], Data, Acc) when is_list(Rules) ->
    Acc2 = normalize(Rules, Data, Acc),
    normalize(Rules2, Data, Acc2);
normalize([K|Rules], Data, Acc) when is_atom(K) ->
    normalize([{K, []}|Rules], Data, Acc);
normalize([K|Rules], Data, Acc) when is_binary(K) ->
    K2 = binary_to_atom(K, utf8),
    normalize([{K2, []}|Rules], Data, Acc);
normalize([], _Data, Acc) ->
    Acc.

is_required(required)       -> true;
is_required(not_empty_list) -> true;
is_required(default)        -> true;
is_required(_)              -> false.
