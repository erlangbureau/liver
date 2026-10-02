# Rule sets: erlang_standard vs livr_spec

Liver has two built-in rule maps. You select **one set**, or compose **several
in order** (first entry has the highest priority on name collision).

| | **erlang_standard** (default) | **livr_spec** |
|--|-------------------------------|---------------|
| Module | `liver_standard_rules` | `liver_livr_rules` |
| Purpose | Erlang terms in apps | [LIVR](http://livr-spec.org) / JSON-style input |
| Coercion | No (use `to_*`) | Yes (`<<"10">>` → `10`, …) |
| Select with | omit / `erlang_standard` | `livr_spec` |

## Selection

```erlang
%% Default — erlang_standard only
liver:validate(Schema, Data).

%% Single built-in set
liver:validate(Schema, Data, #{rule_set => livr_spec}).

%% Ordered composition (first wins on collision)
liver:validate(Schema, Data,
               #{rule_set => [erlang_standard, livr_spec]}).
liver:validate(Schema, Data,
               #{rule_set => [livr_spec, erlang_standard]}).

%% Project rules + builtins (3rd / 4th sets…)
liver:add_rule_set(my_app, #{slug => my_app_rules}).
liver:validate(Schema, Data,
               #{rule_set => [my_app, erlang_standard]}).

%% Inline map as a layer
liver:validate(Schema, Data,
               #{rule_set => [#{slug => my_app_rules}, erlang_standard]}).
```

Legacy aliases:

- `#{livr_compatible => true}` → `livr_spec`
- `{mixed, erlang_standard}` → `[erlang_standard, livr_spec]`
- `{mixed, livr_spec}` → `[livr_spec, erlang_standard]`

## When to use which

- **Application / OTP code** → `erlang_standard` (`is_integer`, `is_utf8_binary`, …).
- **LIVR suite / legacy JSON schemas** → `livr_spec`.
- **Project-specific rules** → `add_rule_set/2` or an inline map, placed
  first in the list so they override builtins when names collide.

## Examples

```erlang
%% erlang_standard: binary is NOT an integer
liver:validate(#{n => is_integer}, #{n => <<"10">>}).
%% {error, #{n => <<"NOT_INTEGER">>}}

%% livr_spec name is unknown in default mode
liver:validate(#{n => integer}, #{n => <<"10">>}).
%% {error, ...}

liver:validate(#{n => integer}, #{n => <<"10">>}, #{rule_set => livr_spec}).
%% {ok, #{n => 10}}

%% In livr_spec mode alone, erlang_standard names are unavailable
liver:validate(#{n => is_integer}, #{n => 10}, #{rule_set => livr_spec}).
%% {error, ...}

%% Compose both: LIVR coercion for `integer`, Erlang check for `is_pos_integer`
liver:validate(#{n => integer, m => is_pos_integer},
               #{n => <<"10">>, m => 3},
               #{rule_set => [livr_spec, erlang_standard]}).
%% {ok, #{n => 10, m => 3}}
```

## Compatibility tests

`livr_rules_SUITE` validates with `#{rule_set => livr_spec}` against
`tests/cases/livr/`.

## Historical note

Older Liver versions exposed a small `liver_strict_rules` set alongside LIVR
defaults. That became `liver_standard_rules` / `erlang_standard` and is now
the default. The validate option `strict` (reject unknown fields) is unrelated
to the rule set.
