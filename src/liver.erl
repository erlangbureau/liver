-module(liver).

%% API
-export([validate/2, validate/3]).
-export([which/1]).
-export([add_rule/2]).

-include("liver.hrl").

-define(IsMap(Data),
    (is_map(Data) orelse is_list(Data))
).

%% API
validate(Schema, Data) ->
    validate(Schema, Data, #{}).

validate(Schema, In, Opts)
        when ?IsMap(Schema) andalso ?IsMap(In) andalso ?IsMap(Opts) ->
    SchemaKeys  = liver_maps:keys(Schema),
    DataKeys    = liver_maps:keys(In),
    Keys        = sift(SchemaKeys, DataKeys, []),
    ReturnType  = get_return_type(Opts, In),
    Out         = liver_maps:new(ReturnType),
    Errors      = liver_maps:new(ReturnType),
    validate(Keys, Schema, In, Out, Errors, Opts);
validate(_Schema, _In, _Opts) ->
    ErrorCode = custom_error_message(format_error),
    {error, ErrorCode}.

which(Rule) ->
    AvailableRules = application:get_env(?MODULE, rules, ?DEFAULT_RULES),
    maps:get(Rule, AvailableRules, undefined_module).

add_rule(Rule, Module) when is_atom(Rule), is_atom(Module) ->
    OldRules = application:get_env(?MODULE, rules, ?DEFAULT_RULES),
    NewRules2 = liver_maps:put(Rule, Module, OldRules),
    application:set_env(?MODULE, rules, NewRules2).

%% internal
validate([{K, intersection}|Keys], Schema, In, Out, Errors, Opts) ->
    %% Key from Schema exists in Data
    Rules = liver_maps:get(K, Schema),
    Value = liver_maps:get(K, In),
    Rules2 = liver_rules:normalize(Rules, In),
    case liver_rules:execute(Rules2, Value, Opts) of
        {ok, Value2} ->
            Out2 = liver_maps:put(K, Value2, Out),
            validate(Keys, Schema, In, Out2, Errors, Opts);
        {error, Err} ->
            ErrorCode = custom_error_message(Err),
            Errors2 = liver_maps:put(K, ErrorCode, Errors),
            validate(Keys, Schema, In, Out, Errors2, Opts)
    end;
validate([{K, schema}|Keys], Schema, In, Out, Errors, Opts) ->
    %% Key from Schema doesn't exist in Data
    Rules = liver_maps:get(K, Schema),
    Rules2 = liver_rules:normalize(Rules, In),
    case liver_rules:has_required(Rules2) of
        true ->
            case liver_rules:execute(Rules2, Opts) of
                {ok, Value2} ->
                    Out2 = liver_maps:put(K, Value2, Out),
                    validate(Keys, Schema, In, Out2, Errors, Opts);
                {error, Err} ->
                    ErrorCode = custom_error_message(Err),
                    Errors2 = liver_maps:put(K, ErrorCode, Errors),
                    validate(Keys, Schema, In, Out, Errors2, Opts)
            end;
        false ->
            validate(Keys, Schema, In, Out, Errors, Opts)
    end;
validate([{K, data}|Keys], Schema, In, Out, Errors, Opts) ->
    %% Key from Data doesn't exist in Schema
    case liver_maps:get(strict, Opts, false) of
        false ->
            %% Strict validation disabled
            validate(Keys, Schema, In, Out, Errors, Opts);
        true ->
            %% Strict validation enabled
            ErrorCode = custom_error_message(unannounced),
            Errors2 = liver_maps:put(K, ErrorCode, Errors),
            validate(Keys, Schema, In, Out, Errors2, Opts)
    end;
validate([], _Schema, _In, Out, Errors, _Opts) ->
    case liver_maps:is_empty(Errors) of
        true ->
            {ok, liver_maps:reverse(Out)};
        false ->
            {error, liver_maps:reverse(Errors)}
    end.

sift([K|SchemaKeys], DataKeys, Acc) ->
    case lists:member(K, DataKeys) of
        true ->
            DataKeys2 = lists:delete(K, DataKeys),
            Acc2 = [{K, intersection}|Acc],
            sift(SchemaKeys, DataKeys2, Acc2);
        false ->
            Acc2 = [{K, schema}|Acc],
            sift(SchemaKeys, DataKeys, Acc2)
    end;
sift([], [K|DataKeys], Acc) ->
    Acc2 = [{K, data}|Acc],
    sift([], DataKeys, Acc2);
sift([], [], Acc) ->
    lists:reverse(Acc).

get_return_type(Opts, InData) ->
    case liver_maps:get(return, Opts, as_is) of
        map         -> map;
        propllist   -> proplist;
        as_is       -> liver_maps:type(InData)
    end.

custom_error_message(Code) ->
    maps:get(Code, ?DEFAULT_LIVR_ERRORS, Code).
