# Liver
[![Build Status](https://github.com/erlangbureau/liver/actions/workflows/ci.yml/badge.svg)](https://github.com/erlangbureau/liver/actions)
[![Coverage Status](https://coveralls.io/repos/github/erlangbureau/liver/badge.svg?branch=master)](https://coveralls.io/github/erlangbureau/liver?branch=master)

## Summary

Liver is a lightweight Erlang/OTP data validator. By default it uses
**standard rules** tailored to Erlang terms (no silent type coercion). It also
implements the [LIVR](http://livr-spec.org) rule set for JSON-style / legacy
schemas when you opt in.

| Docs | |
|------|--|
| [Standard rules](doc/standard_rules.md) | Default rule reference |
| [LIVR vs standard](doc/livr_vs_standard.md) | How to choose a rule set |
| [Changelog](CHANGELOG.md) | Releases and breaking changes |

## Table of Contents
* [Description](#description)
* [Getting Started](#getting-started)
* [Usage Examples](#usage-examples)
* [Rule sets](#rule-sets)
* [Exports](#exports)
* [License](#license)

## Description

**LIVR-inspired design:**

1. Declarative rules, many per field
2. All field errors returned together
3. Nested structures supported
4. Stable error codes (not free-form messages)
5. Easy to add project-specific rules

**Liver-specific:**

1. **Standard rules by default** — Erlang types (`is_integer`, `is_utf8_binary`,
   `is_atom`, …) without silent coercion; explicit `to_*` converters
2. **LIVR rule set on demand** — `#{rule_set => livr_spec}` for spec-compatible
   schemas (and the upstream LIVR test suite)
3. Optional **mixed** rule sets with explicit collision priority
4. `strict` option — reject fields not described in the schema
5. Maps and proplists as input/output (`return => map | proplist | as_is`)
6. List as root value (validate a list of terms/objects)
7. Custom error codes and `add_rule/2` for extensions

## Getting Started

1. Add as a dependency (pin a release tag):

  * **rebar** — `rebar.config`:
  ```erl
{deps, [
    {liver, {git, "https://github.com/erlangbureau/liver.git", {tag, "1.0.0"}}}
]}.
```

  * **erlang.mk**:
```erl
DEPS = liver
dep_liver = git https://github.com/erlangbureau/liver.git 1.0.0
```

2. Add `liver` to `applications` in your `.app.src`.

3. Validate data, or register your own rules with `liver:add_rule/2`.

> **Upgrading from 0.9.x:** since the first version (2017-11-21) Liver was
> LIVR-compatible and stayed pre-1.0 until an Erlang-native default was ready.
> That default is now `erlang_standard`. LIVR schemas need
> `#{rule_set => livr_spec}` (or `livr_compatible => true`).
> Details: [CHANGELOG.md](CHANGELOG.md#100---2026-10-03).

## Usage Examples

### Standard rules (default)

```erlang
1> Schema = #{
       name => [required, is_utf8_binary],
       age  => [required, is_pos_integer],
       role => [{one_of_terms, [[admin, user]]}]
   }.
2> liver:validate(Schema, #{name => <<"Ann">>, age => 30, role => admin}).
{ok,#{age => 30,name => <<"Ann">>,role => admin}}

3> %% No silent coercion: binary is not an integer
3> liver:validate(#{n => is_integer}, #{n => <<"10">>}).
{error,#{n => <<"NOT_INTEGER">>}}

4> %% Convert explicitly, then check
4> liver:validate(#{n => [to_integer, is_pos_integer]}, #{n => <<"10">>}).
{ok,#{n => 10}}
```

### Nested map

```erlang
5> Schema = #{
       address => [required, {nested_map, #{
           country => [required, is_utf8_binary],
           zip => is_pos_integer
       }}]
   }.
6> liver:validate(Schema, #{
       address => #{country => <<"UA">>, zip => 12345, extra => ignored}
   }).
{ok,#{address => #{country => <<"UA">>,zip => 12345}}}
```

### Unknown fields (`strict`)

```erlang
7> liver:validate(#{a => required}, #{a => 1, b => 2}, #{strict => true}).
{error,#{b => <<"UNKNOWN_FIELD">>}}
```

### LIVR rule set (opt-in)

Use when you need LIVR names and JSON-oriented coercion:

```erlang
8> Schema = #{
       <<"zip">> => [required, positive_integer],
       <<"street">> => [required, string]
   }.
9> liver:validate(Schema, #{
       <<"zip">> => <<"12345">>,
       <<"street">> => <<"Main">>
   }, #{rule_set => livr_spec}).
{ok,#{<<"street">> => <<"Main">>,<<"zip">> => 12345}}
```

## Rule sets

| `rule_set` | Behaviour |
|------------|-----------|
| `erlang_standard` (default) | Only `liver_standard_rules` |
| `livr_spec` | Only `liver_livr_rules` |
| `[Set1, Set2, …]` | Compose sets; **first wins** on the same rule name |
| `#{Rule => Module}` | Inline custom rule map |
| `{mixed, erlang_standard}` | Alias for `[erlang_standard, livr_spec]` |
| `{mixed, livr_spec}` | Alias for `[livr_spec, erlang_standard]` |

Each list entry may be a built-in atom, a name from `liver:add_rule_set/2`,
or an inline map.

```erlang
liver:add_rule_set(my_app, #{slug => my_app_rules}).
liver:validate(Schema, Data,
               #{rule_set => [my_app, erlang_standard, livr_spec]}).
```

`#{livr_compatible => true}` is an alias for `#{rule_set => livr_spec}`.

Details: [doc/livr_vs_standard.md](doc/livr_vs_standard.md),
[doc/standard_rules.md](doc/standard_rules.md).

## Exports

### `validate/2`

```erlang
validate(Schema, Input) -> {ok, Output} | {error, Errors}

  Schema, Input, Output, Errors = map() | proplist()
```

Equivalent to `validate(Schema, Input, #{})`.

### `validate/3`

```erlang
validate(Schema, Input, Opts) -> {ok, Output} | {error, Errors}

  Opts = map() | proplist()
```

| Option | Default | Description |
|--------|---------|-------------|
| `return` | `as_is` | `as_is` \| `map` \| `proplist` |
| `strict` | `false` | Reject fields not in schema |
| `rule_set` | `erlang_standard` | See [Rule sets](#rule-sets) |
| `livr_compatible` | `false` | Alias for `rule_set => livr_spec` |

### `which/1`, `which/2`

```erlang
which(Rule) -> module() | undefined_module
which(Rule, Opts) -> module() | undefined_module
```

Resolve which module implements `Rule` for the given options.

### `add_rule/2`

```erlang
add_rule(Rule, Module) -> ok
```

Register a custom rule into the `erlang_standard` application rule map
(also used when that set appears in a `rule_set` list).

### `add_rule_set/2`

```erlang
add_rule_set(Name, Rules) -> ok

  Name = atom()
  Rules = #{atom() => module()}
```

Register a named rule map for composition:

```erlang
liver:add_rule_set(billing, #{iban => billing_rules}).
liver:validate(Schema, Data, #{rule_set => [billing, erlang_standard]}).
```

### `custom_error/2`

```erlang
custom_error(ErrorCode, ErrorMessage) -> ok
```

Override a built-in error code message (binary).

## License

Liver is released under the MIT License
