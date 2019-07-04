-module(liver).

%% API
-export([validate/2, validate/3]).
-export([validate_map/3]).
-export([validate_list/3]).
-export([validate_term/3]).
-export([which/1]).
-export([add_rule/2]).
-export([custom_error/2]).

-include("liver.hrl").

-define(IsKV(Data),
    (is_map(Data) orelse is_list(Data))
).

%% API
validate(Schema, Data) ->
    validate(Schema, Data, #{}).

validate(Schema, Data, Opts) ->
    case detect_datatype_by_schema(Schema) of
        jsobject ->
            validate_map(Schema, Data, Opts);
        list ->
            validate_list(Schema, Data, Opts);
        _ ->
            validate_term(Schema, Data, Opts)
    end.

validate_map(Schema, In, Opts)
        when ?IsKV(Schema) andalso ?IsKV(In) andalso ?IsKV(Opts) ->
    SchemaKeys  = liver_maps:keys(Schema),
    DataKeys    = liver_maps:keys(In),
    Keys        = sift(SchemaKeys, DataKeys, []),
    ReturnType  = get_return_type(Opts, In),
    Out         = liver_maps:new(ReturnType),
    Errors      = liver_maps:new(ReturnType),
    validate(Keys, Schema, In, Out, Errors, Opts);
validate_map(_Schema, _In, _Opts) ->
    ErrorMsg = custom_error_message(format_error),
    {error, ErrorMsg}.

validate_list(Schema, Data, Opts) when is_list(Data) andalso ?IsKV(Opts) ->
    Results = [validate_term(Schema, Value, Opts) || Value <- Data],
    case lists:keymember(error, 1, Results) of
        false ->
            ListOfValues2 = [Val || {ok, Val} <- Results],
            {ok, ListOfValues2};
        true ->
            ListOfErrors = [begin
                case Result of
                    {ok, _} -> null;
                    {error, Err} -> Err
                end
            end || Result <- Results],
            {error, ListOfErrors}
    end.

validate_term(Schema, Data, Opts) when ?IsKV(Opts) ->
    case validate_map(#{'$fake_key' => Schema}, #{'$fake_key' => Data}, Opts) of
        {ok, #{'$fake_key' := Val}} ->
            {ok, Val};
        {error, #{'$fake_key' := Err}} ->
            {error, Err}
    end.

which(Rule) ->
    AvailableRules = application:get_env(?MODULE, rules, ?DEFAULT_RULES),
    maps:get(Rule, AvailableRules, undefined_module).

add_rule(Rule, Module) when is_atom(Rule), is_atom(Module) ->
    OldRules = application:get_env(?MODULE, rules, ?DEFAULT_RULES),
    NewRules = liver_maps:put(Rule, Module, OldRules),
    application:set_env(?MODULE, rules, NewRules).

custom_error(ErrCode, ErrMsg) when is_atom(ErrCode) ->
    OldErrors = application:get_env(?MODULE, errors, ?DEFAULT_ERRORS),
    NewErrors = liver_maps:put(ErrCode, ErrMsg, OldErrors),
    application:set_env(?MODULE, errors, NewErrors).


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
            ErrorCode = custom_error_message(unknown_field),
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
        proplist    -> proplist;
        as_is       -> liver_maps:type(InData)
    end.

custom_error_message(Code) ->
    Errors = application:get_env(?MODULE, errors, ?DEFAULT_ERRORS),
    maps:get(Code, Errors, Code).

detect_datatype_by_schema(Schema) when is_map(Schema) ->
    jsobject;
detect_datatype_by_schema(Schema) when is_list(Schema) ->
    Schema2 = liver_rules:normalize(Schema, []),
    Schema3 = [T || {K, _} = T <- Schema2, is_valid_rule(K)],
    case Schema2 =:= Schema3 of
        true ->
            list;
        false ->
            jsobject
    end;
detect_datatype_by_schema(_Schema) ->
    term.

is_valid_rule(Rule) ->
    which(Rule) /= undefined_module.
