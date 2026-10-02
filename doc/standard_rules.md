# Standard rules

Liver’s default rule set (`liver_standard_rules`) is aimed at **Erlang terms**:
maps, proplists, binaries, atoms, pids, and so on. Predicates **do not** coerce
types. Use explicit `to_*` rules when conversion is intentional.

LIVR specification names (`integer`, `string`, `nested_object`, …) are **not**
registered by default. Switch or compose sets with `rule_set` (see
[livr_vs_standard.md](livr_vs_standard.md)).

## Options

| Option | Default | Meaning |
|--------|---------|---------|
| `return` | `as_is` | `as_is` \| `map` \| `proplist` |
| `strict` | `false` | Reject unknown fields (`UNKNOWN_FIELD`) |
| `rule_set` | `erlang_standard` | Atom, map, or **ordered list** of sets (first wins) |
| `livr_compatible` | `false` | Alias for `rule_set => livr_spec` |

List entries may be `erlang_standard`, `livr_spec`, a name from
`liver:add_rule_set/2`, or an inline `#{Rule => Module}` map.

## Presence / nullability

| Rule | Accepts | Notes |
|------|---------|-------|
| `required` | any present value | Missing field → `REQUIRED` |
| `default` | any / missing | Args: default term |
| `is_null` / `is_not_null` | `null` / not `null` | |
| `is_undefined` / `is_not_undefined` | `undefined` / not | |

## Type predicates

| Rule | Erlang test | Args |
|------|-------------|------|
| `is_integer` | `is_integer/1` | `positive` \| `negative` \| `non_neg` |
| `is_non_neg_integer` | `>= 0` | |
| `is_pos_integer` | `> 0` | |
| `is_float` | `is_float/1` | |
| `is_number` | `is_number/1` | |
| `is_boolean` | `true` \| `false` only | |
| `is_atom` | `is_atom/1` | |
| `is_list` | `is_list/1` | `empty` \| `not_empty` |
| `is_string` | char list | `empty` \| `not_empty` |
| `is_utf8_binary` | valid UTF-8 text binary | `empty` \| `not_empty` |
| `is_binary` | any `binary()` | `empty` \| `not_empty` |
| `is_map` / `is_proplist` | map / `[{_,_}]` | |
| `is_tuple` | `is_tuple/1` | `{size, N}` |
| `is_pid` / `is_ref` / `is_port` | process types | |
| `is_fun` | `is_function/1` | `{arity, A}` |
| `is_term` | always ok | |

## Constraints

| Rule | Args examples |
|------|----------------|
| `one_of_terms` | `[[a, b, 1]]` — strict `=:=` membership |
| `member` | same as `one_of_terms` (list membership) |
| `range` | `[{Min, Max}]` or `[Min, Max]` for numbers |
| `byte_size` | `{eq,N}` \| `{min,N}` \| `{max,N}` \| `{between,Min,Max}` |
| `bit_size` | `{eq,N}` \| `{min,N}` \| `{max,N}` |
| `tuple_size` | `{eq, N}` |
| `map_size` | `{eq,N}` \| `{min,N}` \| `{max,N}` |
| `length` | `{eq,N}` \| `{min,N}` \| `{max,N}` \| `{between,Min,Max}` |

## Converters

| Rule | Behaviour |
|------|-----------|
| `to_integer` / `to_float` | From int/float/binary/list |
| `to_boolean` | Common truthy/falsey forms → boolean |
| `to_string` | → char list |
| `to_utf8_binary` | → UTF-8 text binary |
| `to_binary` | → binary (`iolist_to_binary` for lists) |
| `to_atom` | May **create** atoms (untrusted input risk) |
| `to_existing_atom` | Safe; fails if atom does not exist |
| `to_list` | binary/atom/tuple → list |
| `to_map` / `to_proplist` | proplist ↔ map |

## Special

| Rule | Behaviour |
|------|-----------|
| `email` | Binary/list; Unicode local-part and IDN domains allowed |
| `url` | `http` / `https` via `uri_string:parse/1` (Unicode host ok) |
| `iso_date` | `<<"YYYY-MM-DD">>` or `{Y,M,D}` → `{Y,M,D}` |

## Nested

| Rule | Behaviour |
|------|-----------|
| `nested_map` | Validate nested map/proplist with a nested schema |
| `nested_list` | Validate each list element with a term schema |
| `nested_proplist` | Same as nested map for list/map values |

## Examples

```erlang
Schema = #{
    name => [required, is_utf8_binary],
    age  => [required, is_pos_integer],
    role => [{one_of_terms, [[admin, user]]}]
},
liver:validate(Schema, #{
    name => <<"Ann"/utf8>>,
    age => 30,
    role => admin
}).
%% {ok, #{name => <<"Ann">>, age => 30, role => admin}}
```

```erlang
%% Explicit conversion from external binary input
liver:validate(#{n => [required, to_integer, is_pos_integer]},
               #{n => <<"42">>}).
%% {ok, #{n => 42}}
```
