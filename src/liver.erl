-module(liver).

%% API
-export([validate/2, validate/3]).
-export([validate_map/3]).
-export([validate_list/3]).
-export([validate_term/3]).
-export([which/1, which/2]).
-export([add_rule/2]).
-export([add_rule_set/2]).
-export([add_aliased_rule/1]).
-export([custom_error/2]).
-export([check_schema/1, check_schema/2]).
-export([error_code/2]).

-include("liver.hrl").
-include("liver_rules.hrl").

-define(ALIASED_RULES_KEY, liver_aliased_rules).
-define(CHECKED_SCHEMAS_KEY, liver_checked_schemas).
-define(MAX_CHECKED_SCHEMAS, 1000).

-define(IsKV(Data),
    (is_map(Data) orelse is_list(Data))
).

%% API
validate(Schema, Data) ->
    validate(Schema, Data, #{}).

validate(Schema, Data, Opts) ->
    Opts2 = normalize_opts(Opts),
    ok = check_schema_cached(Schema, Opts, Opts2),
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
    %% In is the containing object of its fields: equal_to_field and the
    %% references of required_if and when read the other fields from it.
    validate_object(Schema, In, In, Opts#{?CONTAINER_OPT => In});
validate_map(_Schema, _In, Opts0) ->
    Opts = try normalize_opts(Opts0) catch _:_ -> #{} end,
    ErrorMsg = error_code(format_error, Opts),
    {error, ErrorMsg}.

validate_list(Schema, Data, Opts0) when is_list(Data) andalso ?IsKV(Opts0) ->
    %% The items have no containing object: references see no fields.
    Opts = (normalize_opts(Opts0))#{?CONTAINER_OPT => #{}},
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
    %% A missing field (e.g. in 'or' branches) is validated as absent: only
    %% required-like rules run, and it stays absent otherwise.
    In = case Data of
        ?MISSED_FIELD_VALUE -> #{};
        _ -> #{'$fake_key' => Data}
    end,
    %% The rules see the containing object of the caller ('or', an aliased
    %% rule, when), not the made-up one.
    Container = maps:get(?CONTAINER_OPT, Opts, #{}),
    %% liver_maps: the result is a proplist with return => proplist
    case validate_object(#{'$fake_key' => Schema}, In, Container, Opts) of
        {ok, Out} ->
            {ok, liver_maps:get('$fake_key', Out, ?MISSED_FIELD_VALUE)};
        {error, Errors} ->
            {error, liver_maps:get('$fake_key', Errors)}
    end.

which(Rule) ->
    which(Rule, #{}).

%% Called for every rule: no normalize_opts/1, which resolves aliased rules.
which(Rule, Opts) when is_map(Opts) ->
    maps:get(Rule, rules_map(Opts), undefined_module);
which(Rule, Opts) when is_list(Opts) ->
    which(Rule, maps:from_list(Opts)).

add_rule(Rule, Module) when is_atom(Rule), is_atom(Module) ->
    OldRules = application:get_env(?MODULE, rules, ?DEFAULT_RULES),
    NewRules = liver_maps:put(Rule, Module, OldRules),
    application:set_env(?MODULE, rules, NewRules),
    _ = reset_checked_schemas(),
    ok.

%% Register a named rule map for use in rule_set lists, e.g.
%%   liver:add_rule_set(my_app, #{my_rule => my_mod}),
%%   liver:validate(Schema, Data, #{rule_set => [my_app, erlang_standard]}).
add_rule_set(Name, Rules) when is_atom(Name), is_map(Rules) ->
    Sets = application:get_env(?MODULE, rule_sets, #{}),
    application:set_env(?MODULE, rule_sets, maps:put(Name, Rules, Sets)),
    _ = reset_checked_schemas(),
    ok.

%% Register a LIVR aliased rule: a rule defined by other rules, e.g.
%%   liver:add_aliased_rule(#{name => adult_age,
%%                            rules => [positive_integer, #{min_number => 18}],
%%                            error => <<"WRONG_AGE">>}).
%% Keys may be binaries too (decoded JSON), `error' is optional.
%% Stored in a persistent_term, as validate/3 reads it on every call.
add_aliased_rule(Alias) when is_map(Alias); is_list(Alias) ->
    {Name, Rules, Error} = parse_aliased_rule(Alias),
    Aliases = persistent_term:get(?ALIASED_RULES_KEY, #{}),
    persistent_term:put(?ALIASED_RULES_KEY,
                        maps:put(Name, {Rules, Error}, Aliases)),
    _ = reset_checked_schemas(),
    ok.

custom_error(ErrCode, ErrMsg) when is_atom(ErrCode) ->
    OldErrors = application:get_env(?MODULE, errors, ?DEFAULT_ERRORS),
    NewErrors = liver_maps:put(ErrCode, ErrMsg, OldErrors),
    application:set_env(?MODULE, errors, NewErrors).

%% Check a schema without data, e.g. once at start-up: unknown rules and,
%% for the rules that check them (the livr_spec set), wrong arguments.
%% validate/3 raises error(Reason) for the same errors, see
%% check_schema_cached/3.
check_schema(Schema) ->
    check_schema(Schema, #{}).

check_schema(Schema, Opts0) ->
    Opts = normalize_opts(Opts0),
    try
        case detect_datatype_by_schema(Schema, Opts) of
            jsobject -> check_fields(Schema, Opts);
            _ -> check_rules(Schema, Opts)
        end
    catch
        throw:{schema_error, Reason} -> {error, Reason}
    end.

%% erlang_standard: lowercase atoms (`not_integer`).
%% livr_spec: LIVR binaries (`<<"NOT_INTEGER">>`).
%% `custom_error/2` overrides still apply to both.
%% Exported for rules that return an error tree, e.g. a code per key.
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

%% internal

%% The schema check of validate/3: error(Reason) for a schema error, e.g.
%% error({unknown_rule, Rule}), also when no data reaches the rule. The
%% schemas that passed (up to ?MAX_CHECKED_SCHEMAS, with their options) are
%% kept in persistent terms, so a schema is checked once, until a rule, a
%% rule set or an aliased rule is registered.
check_schema_cached(Schema, Opts, NormalizedOpts) ->
    {Generation, Count} = checked_schemas(),
    Key = {?CHECKED_SCHEMAS_KEY, Generation, Schema, Opts},
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            case check_schema(Schema, NormalizedOpts) of
                ok ->
                    case counters:get(Count, 1) < ?MAX_CHECKED_SCHEMAS of
                        true ->
                            counters:add(Count, 1, 1),
                            persistent_term:put(Key, true);
                        false ->
                            ok
                    end;
                {error, Reason} ->
                    error(Reason)
            end
    end.

checked_schemas() ->
    case persistent_term:get(?CHECKED_SCHEMAS_KEY, undefined) of
        undefined -> reset_checked_schemas();
        Checked -> Checked
    end.

%% A new generation of checked schemas; the old one is erased.
reset_checked_schemas() ->
    {Generation, _Count} = Checked = {make_ref(), counters:new(1, [])},
    persistent_term:put(?CHECKED_SCHEMAS_KEY, Checked),
    _ = [persistent_term:erase(Key)
         || {{?CHECKED_SCHEMAS_KEY, G, _, _} = Key, _} <- persistent_term:get(),
            G =/= Generation],
    Checked.

validate_object(Schema, In, Container, Opts) ->
    SchemaKeys  = liver_maps:keys(Schema),
    DataKeys    = liver_maps:keys(In),
    Keys        = sift(SchemaKeys, DataKeys, []),
    ReturnType  = get_return_type(Opts, In),
    Out         = liver_maps:new(ReturnType),
    Errors      = liver_maps:new(ReturnType),
    validate(Keys, Schema, In, Container, Out, Errors, Opts).

validate([{K, intersection}|Keys], Schema, In, Container, Out, Errors, Opts) ->
    %% Key from Schema exists in Data
    Rules = liver_maps:get(K, Schema),
    Value = liver_maps:get(K, In),
    Rules2 = liver_rules:normalize(Rules, Container),
    case liver_rules:execute(Rules2, Value, Opts) of
        {ok, Value2} ->
            Out2 = liver_maps:put(K, Value2, Out),
            validate(Keys, Schema, In, Container, Out2, Errors, Opts);
        {error, Err} ->
            ErrorCode = error_code(Err, Opts),
            Errors2 = liver_maps:put(K, ErrorCode, Errors),
            validate(Keys, Schema, In, Container, Out, Errors2, Opts)
    end;
validate([{K, schema}|Keys], Schema, In, Container, Out, Errors, Opts) ->
    %% Key from Schema doesn't exist in Data
    Rules = liver_maps:get(K, Schema),
    Rules2 = liver_rules:normalize(Rules, Container),
    case liver_rules:has_required(Rules2, Opts) of
        true ->
            case liver_rules:execute(Rules2, Opts) of
                {ok, ?MISSED_FIELD_VALUE} ->
                    %% e.g. an 'or' branch without required passed
                    validate(Keys, Schema, In, Container, Out, Errors, Opts);
                {ok, Value2} ->
                    Out2 = liver_maps:put(K, Value2, Out),
                    validate(Keys, Schema, In, Container, Out2, Errors, Opts);
                {error, Err} ->
                    ErrorCode = error_code(Err, Opts),
                    Errors2 = liver_maps:put(K, ErrorCode, Errors),
                    validate(Keys, Schema, In, Container, Out, Errors2, Opts)
            end;
        false ->
            validate(Keys, Schema, In, Container, Out, Errors, Opts)
    end;
validate([{K, data}|Keys], Schema, In, Container, Out, Errors, Opts) ->
    %% Key from Data doesn't exist in Schema
    case liver_maps:get(strict, Opts, false) of
        false ->
            %% Strict validation disabled
            validate(Keys, Schema, In, Container, Out, Errors, Opts);
        true ->
            %% Strict validation enabled
            ErrorCode = error_code(unknown_field, Opts),
            Errors2 = liver_maps:put(K, ErrorCode, Errors),
            validate(Keys, Schema, In, Container, Out, Errors2, Opts)
    end;
validate([], _Schema, _In, _Container, Out, Errors, _Opts) ->
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
    which(Rule, Opts) /= undefined_module
        orelse maps:is_key(Rule, maps:get(?ALIASED_RULES_OPT, Opts, #{})).

parse_aliased_rule(Alias) ->
    Name = case alias_field(name, Alias) of
        Bin when is_binary(Bin), Bin =/= <<>> -> binary_to_atom(Bin, utf8);
        Atom when is_atom(Atom), Atom =/= undefined -> Atom;
        _ -> error({invalid_aliased_rule, Alias})
    end,
    Rules = case alias_field(rules, Alias) of
        undefined -> error({invalid_aliased_rule, Alias});
        Rules0 -> Rules0
    end,
    {Name, Rules, alias_field(error, Alias)}.

alias_field(Key, Alias) ->
    case liver_maps:get(Key, Alias, undefined) of
        undefined -> liver_maps:get(atom_to_binary(Key, utf8), Alias, undefined);
        Value -> Value
    end.

normalize_opts(#{?ALIASED_RULES_OPT := _} = Opts) ->
    %% already normalized (nested validation)
    Opts;
normalize_opts(Opts) when is_map(Opts) ->
    resolve_aliased_rules(Opts);
normalize_opts(Opts) when is_list(Opts) ->
    resolve_aliased_rules(maps:from_list(Opts)).

%% Resolve the aliased rules once per call, so that a rule call costs at
%% most one map lookup: the registered ones and the `aliased_rules' option
%% (it wins, the first alias of a name in it too). Without any, the options
%% stay as they are.
resolve_aliased_rules(Opts) ->
    Registered = persistent_term:get(?ALIASED_RULES_KEY, #{}),
    case maps:get(aliased_rules, Opts, []) of
        [] when map_size(Registered) =:= 0 ->
            Opts;
        List ->
            PerCall = [begin
                {Name, Rules, Error} = parse_aliased_rule(Alias),
                {Name, {Rules, Error}}
            end || Alias <- lists:reverse(List)],
            Aliases = maps:merge(Registered, maps:from_list(PerCall)),
            Opts#{?ALIASED_RULES_OPT => Aliases}
    end.

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

%% check_schema/2
check_fields(Schema, Opts) when ?IsKV(Schema) ->
    case Schema of
        [{}] -> ok;
        _ -> lists:foreach(fun
                 ({_Field, Rules}) -> check_rules(Rules, Opts);
                 (_) -> throw({schema_error, {invalid_schema, Schema}})
             end, to_list(Schema))
    end;
check_fields(Schema, _Opts) ->
    throw({schema_error, {invalid_schema, Schema}}).

check_rules(Rules, Opts) ->
    Rules2 = try liver_rules:normalize(Rules, #{})
             catch error:_ -> throw({schema_error, {invalid_rules, Rules}})
             end,
    lists:foreach(fun({Rule, Args}) -> check_rule(Rule, Args, Opts) end, Rules2).

check_rule(Rule, _Args, #{?ALIASED_RULES_OPT := Aliases} = Opts)
        when is_map_key(Rule, Aliases) ->
    {AliasRules, _Error} = maps:get(Rule, Aliases),
    check_rules(AliasRules, Opts);
check_rule(Rule, Args, Opts) ->
    case which(Rule, Opts) of
        undefined_module ->
            throw({schema_error, {unknown_rule, Rule}});
        Module ->
            _ = code:ensure_loaded(Module),
            case erlang:function_exported(Module, check_args, 2) of
                true -> check_nested(Module:check_args(Rule, Args), Rule, Args, Opts);
                false -> ok
            end
    end.

check_nested(ok, _Rule, _Args, _Opts) ->
    ok;
check_nested({ok, Nested}, _Rule, _Args, Opts) ->
    lists:foreach(fun
        ({schema, Schema}) -> check_fields(Schema, Opts);
        ({rules, Rules}) -> check_rules(Rules, Opts)
    end, Nested);
check_nested(error, Rule, Args, _Opts) ->
    throw({schema_error, {invalid_args, Rule, Args}}).

to_list(Map) when is_map(Map) -> maps:to_list(Map);
to_list(List) when is_list(List) -> List.
