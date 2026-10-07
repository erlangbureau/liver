-module(liver).

%% API
-export([validate/2, validate/3]).
-export([validate_map/3]).
-export([validate_list/3]).
-export([validate_term/3]).
-export([which/1, which/2]).
-export([add_rule/2]).
-export([add_rule_set/2]).
-export([custom_error/2]).

-include("liver.hrl").

-define(IsKV(Data),
    (is_map(Data) orelse is_list(Data))
).

%% API
validate(Schema, Data) ->
    validate(Schema, Data, #{}).

validate(Schema, Data, Opts) ->
    Opts2 = normalize_opts(Opts),
    case detect_datatype_by_schema(Schema, Opts2) of
        jsobject ->
            validate_map(Schema, Data, Opts2);
        list ->
            validate_list(Schema, Data, Opts2);
        _ ->
            validate_term(Schema, Data, Opts2)
    end.

validate_map(Schema, In, Opts0)
        when ?IsKV(Schema) andalso ?IsKV(In) andalso ?IsKV(Opts0) ->
    Opts = normalize_opts(Opts0),
    SchemaKeys  = liver_maps:keys(Schema),
    DataKeys    = liver_maps:keys(In),
    Keys        = sift(SchemaKeys, DataKeys, []),
    ReturnType  = get_return_type(Opts, In),
    Out         = liver_maps:new(ReturnType),
    Errors      = liver_maps:new(ReturnType),
    validate(Keys, Schema, In, Out, Errors, Opts);
validate_map(_Schema, _In, Opts0) ->
    Opts = try normalize_opts(Opts0) catch _:_ -> #{} end,
    ErrorMsg = error_code(format_error, Opts),
    {error, ErrorMsg}.

validate_list(Schema, Data, Opts0) when is_list(Data) andalso ?IsKV(Opts0) ->
    Opts = normalize_opts(Opts0),
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

validate_term(Schema, Data, Opts0) when ?IsKV(Opts0) ->
    Opts = normalize_opts(Opts0),
    case validate_map(#{'$fake_key' => Schema}, #{'$fake_key' => Data}, Opts) of
        {ok, #{'$fake_key' := Val}} ->
            {ok, Val};
        {error, #{'$fake_key' := Err}} ->
            {error, Err}
    end.

which(Rule) ->
    which(Rule, #{}).

which(Rule, Opts) when is_map(Opts); is_list(Opts) ->
    maps:get(Rule, rules_map(normalize_opts(Opts)), undefined_module).

add_rule(Rule, Module) when is_atom(Rule), is_atom(Module) ->
    OldRules = application:get_env(?MODULE, rules, ?DEFAULT_RULES),
    NewRules = liver_maps:put(Rule, Module, OldRules),
    application:set_env(?MODULE, rules, NewRules).

%% Register a named rule map for use in rule_set lists, e.g.
%%   liver:add_rule_set(my_app, #{my_rule => my_mod}),
%%   liver:validate(Schema, Data, #{rule_set => [my_app, erlang_standard]}).
add_rule_set(Name, Rules) when is_atom(Name), is_map(Rules) ->
    Sets = application:get_env(?MODULE, rule_sets, #{}),
    application:set_env(?MODULE, rule_sets, maps:put(Name, Rules, Sets)).

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
            ErrorCode = error_code(Err, Opts),
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
                    ErrorCode = error_code(Err, Opts),
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
            ErrorCode = error_code(unknown_field, Opts),
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

%% erlang_standard: lowercase atoms (`not_integer`).
%% livr_spec: LIVR binaries (`<<"NOT_INTEGER">>`).
%% `custom_error/2` overrides still apply to both.
error_code(Code, Opts) when is_atom(Code) ->
    Errors = application:get_env(?MODULE, errors, ?DEFAULT_ERRORS),
    case livr_error_codes(Opts) of
        true ->
            maps:get(Code, Errors, Code);
        false ->
            Default = maps:get(Code, ?DEFAULT_ERRORS, undefined),
            case maps:find(Code, Errors) of
                {ok, Default} -> Code;
                {ok, Custom} -> Custom;
                error -> Code
            end
    end;
error_code(Other, _Opts) ->
    Other.

livr_error_codes(Opts) ->
    case rule_set(Opts) of
        livr_spec -> true;
        {mixed, livr_spec} -> true;
        [livr_spec | _] -> true;
        _ -> false
    end.

detect_datatype_by_schema(Schema, _Opts) when is_map(Schema) ->
    jsobject;
detect_datatype_by_schema(Schema, Opts) when is_list(Schema) ->
    Schema2 = liver_rules:normalize(Schema, []),
    Schema3 = [T || {K, _} = T <- Schema2, is_valid_rule(K, Opts)],
    case Schema2 =:= Schema3 of
        true ->
            list;
        false ->
            jsobject
    end;
detect_datatype_by_schema(_Schema, _Opts) ->
    term.

is_valid_rule(Rule, Opts) ->
    which(Rule, Opts) /= undefined_module.

normalize_opts(Opts) when is_map(Opts) ->
    Opts;
normalize_opts(Opts) when is_list(Opts) ->
    maps:from_list(Opts).

%% Active rule map for this call.
%%
%% rule_set values:
%%   erlang_standard | livr_spec     — built-in single sets
%%   #{Rule => Module}               — inline custom map
%%   NamedAtom                       — from liver:add_rule_set/2
%%   [Set1, Set2, ...]               — ordered composition; **first wins**
%%   {mixed, erlang_standard}        — alias for [erlang_standard, livr_spec]
%%   {mixed, livr_spec}              — alias for [livr_spec, erlang_standard]
%%
%% livr_compatible => true is an alias for rule_set => livr_spec.
rules_map(Opts) ->
    compose_rule_sets(rule_set(Opts)).

rule_set(Opts) ->
    case maps:find(rule_set, Opts) of
        {ok, Set} ->
            Set;
        error ->
            case maps:get(livr_compatible, Opts, false) of
                true -> livr_spec;
                false -> erlang_standard
            end
    end.

compose_rule_sets({mixed, erlang_standard}) ->
    compose_rule_sets([erlang_standard, livr_spec]);
compose_rule_sets({mixed, livr_spec}) ->
    compose_rule_sets([livr_spec, erlang_standard]);
compose_rule_sets(Set) when is_atom(Set); is_map(Set) ->
    resolve_rule_set(Set);
compose_rule_sets(Sets) when is_list(Sets), Sets =/= [] ->
    %% First entry has highest priority on name collision.
    lists:foldl(fun(Set, Acc) ->
        maps:merge(resolve_rule_set(Set), Acc)
    end, #{}, Sets);
compose_rule_sets([]) ->
    error(empty_rule_set);
compose_rule_sets(Other) ->
    error({invalid_rule_set, Other}).

resolve_rule_set(erlang_standard) ->
    application:get_env(?MODULE, rules, ?ERLANG_STANDARD_RULES);
resolve_rule_set(livr_spec) ->
    ?LIVR_SPEC_RULES;
resolve_rule_set(Map) when is_map(Map) ->
    Map;
resolve_rule_set(Name) when is_atom(Name) ->
    Sets = application:get_env(?MODULE, rule_sets, #{}),
    case maps:find(Name, Sets) of
        {ok, Map} when is_map(Map) ->
            Map;
        error ->
            error({unknown_rule_set, Name})
    end.
