-module(oliver).

%% API
-export([validate/2, validate/3]).
-export([get_env/1, get_env/2]).
-export([which/1]).

-include("oliver.hrl").

%% API
validate(Schema, Data) ->
    validate(Schema, Data, #{}).

validate(Schema, InData, Opts) ->
    SchemaKeys  = oliver_maps:keys(Schema),
    SchemaKeys2 = lists:sort(SchemaKeys),
    DataKeys    = oliver_maps:keys(InData),
    DataKeys2   = lists:sort(DataKeys),
    ReturnType  = get_return_type(Opts, InData),
    OutData     = oliver_maps:new(ReturnType),
    ErrData     = oliver_maps:new(ReturnType),
    validate(SchemaKeys2, DataKeys2, Schema, InData, OutData, ErrData, Opts).

which(Rule) ->
    which(Rule, ?DEFAULT_RULES).    %% TODO custom rules

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(oliver, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.

%% internal
validate([K|SchemaKeys], [K|DataKeys], Schema, In, Out, Errors, Opts) ->
    %% Key from Rule exists in Data
    Rules = oliver_maps:get(K, Schema),
    Value = oliver_maps:get(K, In),
    ApplyFun = fun(Rule, RuleArgs, Acc) ->
        apply_rule(Rule, RuleArgs, Acc, Opts, In)
    end,
    case rules_fold(ApplyFun, {ok, Value}, Rules) of
        {ok, Value2} ->
            Out2 = oliver_maps:put(K, Value2, Out),
            validate(SchemaKeys, DataKeys, Schema, In, Out2, Errors, Opts);
        {error, Err} ->
            Errors2 = oliver_maps:put(K, Err, Errors),
            validate(SchemaKeys, DataKeys, Schema, In, Out, Errors2, Opts)
    end;
validate(SchemaKeys, [K|DataKeys], Schema, In, Out, Errors, Opts) ->
    %% Key from Data doesn't exist in Rule
    case oliver_maps:get(strict, Opts, false) of
        false ->
            %% Strict validation disabled
            validate(SchemaKeys, DataKeys, Schema, In, Out, Errors, Opts);
        true ->
            %% Strict validation enabled
            Errors2 = oliver_maps:put(K, unannounced, Errors),
            validate(SchemaKeys, DataKeys, Schema, In, Out, Errors2, Opts)
    end;
validate([K|SchemaKeys], DataKeys, Schema, In, Out, Errors, Opts) ->
    %% Key from Rule doesn't exist in Data
    Rules = oliver_maps:get(K, Schema),
    IsRequired  = is_rule_exists(required, Rules),
    IsDefault   = is_rule_exists(default, Rules),
    case {IsRequired, IsDefault} of
        {_, true} ->
            Args = oliver_maps:get(default, Rules),
            {ok, Value2} = apply_rule(default, Args, {ok, undefined}, Opts, In),
            Out2 = oliver_maps:put(K, Value2, Out),
            validate(SchemaKeys, DataKeys, Schema, In, Out2, Errors, Opts);
        {false, false} ->
            validate(SchemaKeys, DataKeys, Schema, In, Out, Errors, Opts);
        {true, false} ->
            Errors2 = oliver_maps:put(K, required, Errors),
            validate(SchemaKeys, DataKeys, Schema, In, Out, Errors2, Opts)
    end;
validate([], [], _Schema, _In, Out, Errors, _Opts) ->
    case oliver_maps:is_empty(Errors) of
        true ->
            {ok, Out};
        false ->
            {error, Errors}
    end.

apply_rule(Rule, Args, {ok, Value}, Opts, InData) when is_atom(Rule) ->
    Module = which(Rule),
    try Module:Rule(Args, Value, Opts, InData)
    catch
        error:undef ->
            {error, {unimplemented_rule, Rule}};
        _Type:_Reason ->
            %% TODO check schema and input data and return understandable error
            {error, format_error}
    end;
apply_rule(_Rule, _Args, {error, _} = Err, _Opts, _InData) ->
    Err;
apply_rule(Rule, Args, Acc, Opts, InData) when is_binary(Rule) ->
    AtomRule = binary_to_atom(Rule, utf8),
    apply_rule(AtomRule, Args, Acc, Opts, InData).

which(Rule, AvailableRules) ->
    maps:get(Rule, AvailableRules, undefined).

get_return_type(Opts, InData) ->
    case oliver_maps:get(return, Opts, as_is) of
        map     -> map;
        list    -> list;
        as_is   -> oliver_maps:type(InData)
    end.

is_rule_exists(Key, Atom) when is_atom(Atom) ->
    Key =:= Atom;
is_rule_exists(Key, Rules) ->
    oliver_maps:is_key(Key, Rules).

rules_fold(Fun, Init, Map) when is_map(Map) ->
    maps:fold(Fun, Init, Map);
rules_fold(Fun, Init, List) when is_list(List) ->
    rules_foldl(Fun, Init, List);
rules_fold(Fun, Init, Key) when is_atom(Key) ->
    Fun(Key, [], Init).

rules_foldl(F, Acc, [{K, V}|Tail]) ->
    rules_foldl(F, F(K, V, Acc), Tail);
rules_foldl(F, Acc, [K|Tail]) ->
    rules_foldl(F, F(K, [], Acc), Tail);
rules_foldl(_F, Acc, []) ->
    Acc.
