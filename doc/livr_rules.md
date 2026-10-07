# LIVR rules (`livr_spec`)

Reference for Liver’s **LIVR-compatible** rule set (`liver_livr_rules`), selected
with:

```erlang
liver:validate(Schema, Data, #{rule_set => livr_spec}).
%% or
liver:validate(Schema, Data, #{livr_compatible => true}).
```

Behaviour follows [LIVR](http://livr-spec.org) 2.0: declarative rules per field,
shared error codes, and **type coercion suited to JSON / form input** (see
[Comparing the sets](livr_vs_standard.md)).

Official LIVR examples and the language-independent suite live upstream; Liver
runs that suite under `tests/cases/livr/` via `livr_rules_SUITE`.

## Scheme shape

```erlang
Schema = #{
    <<"field">> => [required, integer],
    <<"name">>  => [required, {min_length, [2]}, {max_length, [50]}]
}.
```

A field value is a single rule atom, a `{Rule, Args}` tuple, a map
`#{Rule => Args}`, or a **list** of those (applied in order). Rules may change
the value for the next rule (`trim`, `integer`, `nested_object`, …).

Fields not listed in the schema are **dropped** from a successful result
(unless you only care about errors). Use Liver’s `strict` option if unknown
fields must fail validation.

## Common rules

| Rule | Args | Notes |
|------|------|--------|
| `required` | — | Field must be present and not empty / `null` / `undefined` |
| `not_empty` | — | Value must not be `<<>>` |
| `not_empty_list` | — | Non-empty list (not `[]`, not empty binary) |
| `any_object` | — | Map or proplist (or empty binary, kept as-is) |

## String rules

| Rule | Args | Notes |
|------|------|--------|
| `string` | — | Binary, or **number coerced** to binary |
| `eq` | Value | Equal to constant; number↔binary coercion when comparing |
| `one_of` | `[V1, V2, …]` | Must match one allowed value (`eq` semantics) |
| `min_length` | `N` | UTF-8 length ≥ N (numbers coerced to binary first) |
| `max_length` | `N` | UTF-8 length ≤ N |
| `length_equal` | `N` | Exact length |
| `length_between` | `[Min, Max]` | Inclusive length range |
| `like` | Pattern \| `[Pattern]` \| `[Pattern, 'i']` | Regex; optional case-insensitive |

## Numeric rules

These rules treat **JSON-style strings** as numbers when needed
(`<<"10">>` → `10`, `<<"1.5">>` → `1.5`).

| Rule | Args | Notes |
|------|------|--------|
| `integer` | — | Integer, or binary parsed as integer |
| `positive_integer` | — | Integer `> 0` (binary allowed) |
| `decimal` | — | Float, or binary parsed as float (integers as binary/number rejected as “not decimal” per LIVR) |
| `positive_decimal` | — | Decimal `> 0` |
| `min_number` | `N` | Value ≥ N |
| `max_number` | `N` | Value ≤ N |
| `number_between` | `[Min, Max]` | Inclusive numeric range |

## Special rules

| Rule | Args | Notes |
|------|------|--------|
| `email` | — | Basic email check on a binary |
| `url` | — | HTTP(S) URL check on a binary |
| `iso_date` | — | `YYYY-MM-DD` binary → Erlang `{Y,M,D}` date |
| `equal_to_field` | FieldName | Must equal another field’s value |

## Meta rules

| Rule | Args | Notes |
|------|------|--------|
| `nested_object` | NestedSchema | Validate a nested map/proplist |
| `list_of` | Rules | Each list element validated with Rules |
| `list_of_objects` | NestedSchema | List of objects with the same schema |
| `list_of_different_objects` | `[Discriminator, Schemas]` | Pick nested schema by discriminator field |
| `variable_object` | `[Discriminator, Schemas]` | Single object; schema depends on discriminator |
| `'or'` | `[Rules1, Rules2, …]` | First successful alternative wins |

## Modifiers (filters)

| Rule | Args | Notes |
|------|------|--------|
| `trim` | — | Trim whitespace (numbers coerced to binary first) |
| `to_lc` | — | Lowercase binary / number→binary |
| `to_uc` | — | Uppercase |
| `remove` | Chars | Remove listed characters |
| `leave_only` | Chars | Keep only listed characters |
| `default` | Value | If field missing / empty, use Value |

## Examples

```erlang
Schema = #{
    <<"email">> => [required, email],
    <<"age">>   => [required, positive_integer],
    <<"name">>  => [required, {min_length, [1]}, trim]
},
Data = #{
    <<"email">> => <<"a@b.co">>,
    <<"age">>   => <<"30">>,
    <<"name">>  => <<" Ann ">>
},
liver:validate(Schema, Data, #{rule_set => livr_spec}).
%% {ok, #{<<"email">> => <<"a@b.co">>,
%%        <<"age">> => 30,
%%        <<"name">> => <<"Ann">>}}
```

```erlang
Schema = #{
    <<"address">> => [{nested_object, #{
        <<"country">> => [required, string],
        <<"zip">> => positive_integer
    }}]
},
liver:validate(Schema, #{
    <<"address">> => #{<<"country">> => <<"UA">>, <<"zip">> => <<"01001">>}
}, #{rule_set => livr_spec}).
```

## Error codes

Typical codes (binaries): `REQUIRED`, `CANNOT_BE_EMPTY`, `TOO_LONG`, `TOO_SHORT`,
`TOO_HIGH`, `TOO_LOW`, `NOT_ALLOWED_VALUE`, `NOT_INTEGER`, `NOT_POSITIVE_INTEGER`,
`NOT_DECIMAL`, `NOT_POSITIVE_DECIMAL`, `NOT_NUMBER`, `WRONG_EMAIL`, `WRONG_URL`,
`WRONG_DATE`, `FIELDS_NOT_EQUAL`, `FORMAT_ERROR`, …

Override messages with `liver:custom_error/2`.

## See also

- [Comparing LIVR and standard](livr_vs_standard.md)
- [Standard rules](standard_rules.md)
- Upstream: [livr-spec.org](http://livr-spec.org)
