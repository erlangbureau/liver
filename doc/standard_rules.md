# Standard rules (`erlang_standard`)

Reference for Liver’s **default** rule set (`liver_standard_rules`).

```erlang
%% Default — no extra options needed
liver:validate(Schema, Data).

liver:validate(Schema, Data, #{rule_set => erlang_standard}).
```

These rules target **Erlang terms** already: integers, floats, binaries, atoms,
pids, maps, proplists. Predicates **check types**; they do **not** parse JSON
strings for you. When input still arrives as binaries, add an explicit `to_*`
rule in the pipeline.

Why LIVR’s `integer` feels “automatic” and `is_integer` does not is explained in
[Comparing the sets](livr_vs_standard.md).

## Scheme shape

Same as LIVR: field → one rule, `{Rule, Args}`, or a list of rules applied in
order.

```erlang
Schema = #{
    name => [required, is_utf8_binary],
    age  => [required, to_integer, is_pos_integer]
}.
```

## Presence / nullability

| Rule | Args | Notes |
|------|------|--------|
| `required` | — | Field must be present |
| `default` | Value \| `[Value]` | Fill in when missing |
| `is_null` | — | Exactly `null` |
| `is_not_null` | — | Anything except `null` |
| `is_undefined` | — | Exactly `undefined` |
| `is_not_undefined` | — | Anything except `undefined` |

## Type predicates

No coercion: `<<"10">>` fails `is_integer`.

| Rule | Args | Notes |
|------|------|--------|
| `is_integer` | — \| `positive` \| `negative` \| `non_neg` | `is_integer/1` |
| `is_non_neg_integer` | — | `>= 0` |
| `is_pos_integer` | — | `> 0` |
| `is_float` | — | `is_float/1` |
| `is_number` | — | `is_number/1` |
| `is_boolean` | — | Only `true` / `false` |
| `is_atom` | — | `is_atom/1` |
| `is_list` | — \| `empty` \| `not_empty` | |
| `is_string` | — \| `empty` \| `not_empty` | Character list |
| `is_utf8_binary` | — \| `empty` \| `not_empty` | Valid UTF-8 text binary |
| `is_binary` | — \| `empty` \| `not_empty` | Any `binary()` |
| `is_map` | — | |
| `is_proplist` | — | List of `{K,V}` pairs |
| `is_tuple` | — \| `{size, N}` | |
| `is_pid` / `is_ref` / `is_port` | — | |
| `is_fun` | — \| `{arity, A}` | |
| `is_term` | — | Always succeeds |

## Constraints

| Rule | Args examples | Notes |
|------|----------------|--------|
| `one_of_terms` | `[[a, b, 1]]` | Strict `=:=` membership |
| `member` | `[List]` | Same idea as `one_of_terms` |
| `range` | `[{Min,Max}]` \| `[Min,Max]` | Numeric inclusive range |
| `byte_size` | `{eq,N}` \| `{min,N}` \| `{max,N}` \| `{between,Min,Max}` | |
| `bit_size` | `{eq,N}` \| `{min,N}` \| `{max,N}` | |
| `tuple_size` | `{eq, N}` | |
| `map_size` | `{eq,N}` \| `{min,N}` \| `{max,N}` | |
| `length` | `{eq,N}` \| `{min,N}` \| `{max,N}` \| `{between,Min,Max}` | List length |

## Converters

Use when the boundary still speaks JSON/text but the rest of the schema should
see Erlang types.

| Rule | Notes |
|------|--------|
| `to_integer` / `to_float` | From int/float/binary/list |
| `to_boolean` | Common truthy/falsey forms → boolean |
| `to_string` | → character list (`float_to_list(..., [short])` for floats) |
| `to_utf8_binary` | → UTF-8 text binary |
| `to_binary` | → binary (`iolist_to_binary` for lists) |
| `to_atom` | May **create** atoms — avoid on untrusted input |
| `to_existing_atom` | Fails if the atom does not exist yet |
| `to_list` | binary/atom/tuple → list |
| `to_map` / `to_proplist` | proplist ↔ map |

```erlang
liver:validate(#{n => [required, to_integer, is_pos_integer]},
               #{n => <<"42">>}).
%% {ok, #{n => 42}}
```

## Special

| Rule | Notes |
|------|--------|
| `email` | Binary/list. Internationalized addresses (Unicode local-part, IDN hosts). |
| `url` | `http` / `https` only; ASCII/punycode via `uri_string`, Unicode IDN via fallback. |
| `iso_date` | `<<"YYYY-MM-DD">>` or `{Y,M,D}` → `{Y,M,D}` |

```erlang
liver:validate(#{e => email}, #{e => <<"квіточка@пошта.укр"/utf8>>}).
liver:validate(#{u => url}, #{u => <<"https://пошта.укр"/utf8>>}).
```

## Nested

| Rule | Notes |
|------|--------|
| `nested_map` | Nested map/proplist against a nested schema |
| `nested_list` | Each list element against a term schema / rules |
| `nested_proplist` | Nested validation preferring proplist-shaped data |

```erlang
Schema = #{
    address => [required, {nested_map, #{
        country => [required, is_utf8_binary],
        zip => is_pos_integer
    }}]
}.
```

## Options (validator)

| Option | Default | Meaning |
|--------|---------|---------|
| `return` | `as_is` | `as_is` \| `map` \| `proplist` |
| `strict` | `false` | Reject unknown fields (`UNKNOWN_FIELD`) |
| `rule_set` | `erlang_standard` | Atom, map, or ordered list of sets |
| `livr_compatible` | `false` | Alias for `rule_set => livr_spec` |

## See also

- [LIVR rules](livr_rules.md)
- [Comparing the sets](livr_vs_standard.md)
