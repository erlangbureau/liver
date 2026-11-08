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
[Comparing the sets](livr_vs_standard.md)). The set also has the rules of the
[LIVR 2.5](#livr-25-rules).

Official LIVR examples and the language-independent suite live upstream; Liver
runs that suite under `tests/cases/livr/` via `livr_rules_SUITE`, and the 2.5
suite under `tests/cases/livr25/` via `livr25_rules_SUITE`.

Empty values: rules other than `required`, `not_empty_list` and `default` pass
`<<>>` and `null` and keep them as they are.

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
| `decimal` | — | Number, or binary parsed as a number (`<<"10">>` → `10`, `<<"1.5">>` → `1.5`) |
| `positive_decimal` | — | Decimal `> 0` |
| `min_number` | `N` | Value ≥ N |
| `max_number` | `N` | Value ≤ N |
| `number_between` | `[Min, Max]` | Inclusive numeric range |

## Special rules

| Rule | Args | Notes |
|------|------|--------|
| `email` | — | Basic email check on a binary |
| `url` | — | HTTP(S) URL check on a binary: host labels of 1–63 characters, no dot after the host, a port of 1–5 digits up to 65535 (LIVR 2.5) |
| `iso_date` | — \| `#{<<"min">> => B, <<"max">> => B}` | `YYYY-MM-DD` binary → Erlang `{Y,M,D}` date; bounds (LIVR 2.5): a date, `<<"today">>`, `<<"yesterday">>`, `<<"tomorrow">>` (UTC) |
| `equal_to_field` | FieldName | Must equal another field’s value, compared as strings (`<<"10">>` equals `10`). Inside `'or'` and aliases it reads the fields next to the validated one |

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

## LIVR 2.5 rules

The rules added in [LIVR 2.5](https://github.com/koorchik/LIVR/blob/master/drafts/v2.5.md)
(see also [the changes](https://livr-spec.org/changes.html#v2-5)).

Text rules check the string form of a primitive value and output it:
`1.0` → `<<"1">>`, `0.5` → `<<"0.5">>`, `true` → `<<"true">>`. Format and
checksum rules reject booleans with their own code. Objects and lists give
`FORMAT_ERROR`. All rules pass `<<>>`, `null` and a missing field, except
`required_if`, `'when'` and `any`.

| Rule | Args | Notes |
|------|------|--------|
| `boolean` | — | `true`, `false`, `1`, `0`, `<<"1">>`, `<<"0">>`, `<<"true">>`, `<<"false">>` → `true` / `false` |
| `not_one_of` | `[V1, V2, …]` | Not equal to any of them, compared as strings |
| `max_decimals` | `N` | At most N digits after the point, trailing zeros not counted; a binary becomes a number |
| `digits` | — | ASCII digits, kept as a binary (`<<"007">>`) |
| `starts_with`, `ends_with`, `contains` | `[S1, S2, …]` | At least one of the strings, case-sensitive |
| `iso_datetime` | — \| `#{<<"min">> => B, <<"max">> => B}` | Date, time and offset → `<<"YYYY-MM-DDTHH:MM:SS.sssZ">>` in UTC; bounds: a datetime, a date or a relative day |
| `iso_time` | — \| `#{<<"min">> => T, <<"max">> => T}` | `HH:MM[:SS[.fraction]]`; `min` > `max` is a range over midnight |
| `required_if` | `#{Ref => Value \| [Values]}` | `REQUIRED` when all the referenced fields have these values |
| `'when'` | `[Ref, Condition, IfTrue]`, `[Ref, Condition, IfTrue, IfFalse]` or `[Ref, Cases]` | Rules that depend on another field |
| `values`, `keys` | Rules | Every value / key of an object, like `list_of` |
| `any` | — | Any value, kept as it is |
| `list_length` | `N`, `[Min, Max]` or `#{<<"min">> => Min, <<"max">> => Max}` | Number of items |
| `unique` | — or `[Key1, …]` | No duplicate items, or objects with the same keys |
| `split` | Separator | A binary → list of binaries; a list stays as it is |
| `uuid` | — or `[<<"v4">>, …]` | RFC 9562 |
| `ip`, `cidr` | — \| `<<"v4">>` \| `<<"v6">>` | |
| `hostname`, `hex`, `mongo_id`, `hex_color`, `e164`, `semver`, `jwt` | — | |
| `base64` | — \| `<<"url">>` | |
| `digest` | `[<<"md5">>, <<"sha256">>, …]` | Hex digest length |
| `mac` | — \| `48` \| `64` | |
| `luhn`, `credit_card`, `iban` | — | Checksums |

A reference `Ref` is a field of the containing object (the object validated
field by field): `<<"type">>`, a path `<<"address/country">>` (`~1` is `/`,
`~0` is `~`), or an atom for a schema with atom keys. `'when'` is a reserved
word in Erlang, so the rule name needs quotes:

```erlang
Schema = #{
    <<"type">> => [required, #{one_of => [<<"person">>, <<"company">>]}],
    <<"vat">> => #{'when' => [<<"type">>, #{eq => <<"company">>},
                              [required, #{like => <<"^[A-Z]{2}[0-9]{8,12}$">>}],
                              #{eq => <<>>}]}
}.
```

`today`, `yesterday` and `tomorrow` read the clock through `liver_clock`, so
tests can fix it (it is a persistent term, so not for production use):

```erlang
liver_clock:set_time(1792324800000),  % ms since the epoch, UTC
liver_clock:set_time(undefined).      % the system time again
```

### Schema errors

Arguments that do not match a 2.5 rule (`{boolean, 1}`, `#{iso_date => <<"today">>}`,
an unknown option, `[]` as the rules of `'when'`) are schema errors.
`liver:check_schema/2` finds them, and unknown rules, without data:

```erlang
1> liver:check_schema(#{<<"id">> => {uuid, 4}}, #{rule_set => livr_spec}).
{error,{invalid_args,uuid,4}}
2> liver:check_schema(#{<<"id">> => [required, hash]}, #{rule_set => livr_spec}).
{error,{unknown_rule,hash}}
```

`validate/3` runs the same check and raises the reason, e.g.
`error({invalid_args, Rule, Args})` or `error({unknown_rule, Rule})`, also when
no data reaches the rule (a missing field, an empty list, an `'or'` branch that
does not run). It checks a schema once and keeps the schemas that passed.

`null` as the argument (`{"rule": null}` in JSON) is one `null` argument, and
no 2.5 rule takes it: `{boolean, null}` and `{uuid, null}` are schema errors.
LIVR 2.0 ignored the arguments of `iso_date`, so `{iso_date, null}` is the
plain `iso_date`. The other 2.0 rules and custom rules get `null` as their
argument, as before: `{default, null}` sets `null`. `null` in a list or object
of arguments (`{not_one_of, [null]}`, `{list_length, #{<<"min">> => null}}`) is
a schema error too.

## Aliased rules

[LIVR aliases](https://livr-spec.org/rules-aliasing.html) define a new rule from other
rules, as data. Register one with `liver:add_aliased_rule/1`, or pass a list of
them in the `aliased_rules` option of a call:

```erlang
liver:add_aliased_rule(#{
    name => valid_address,
    rules => #{nested_object => #{
        <<"country">> => required,
        <<"zip">> => positive_integer
    }},
    error => <<"WRONG_ADDRESS">>    % optional: replaces the errors of the rules
}),
liver:validate(#{<<"address">> => [required, valid_address]}, Data,
               #{rule_set => livr_spec}).
```

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

LIVR 2.5: `NOT_BOOLEAN`, `TOO_MANY_DECIMALS`, `NOT_DIGITS`, `DATE_TOO_EARLY`,
`DATE_TOO_LATE`, `WRONG_DATETIME`, `DATETIME_TOO_EARLY`, `DATETIME_TOO_LATE`,
`WRONG_TIME`, `TIME_TOO_EARLY`, `TIME_TOO_LATE`, `WRONG_KEY`, `TOO_FEW_ITEMS`,
`TOO_MANY_ITEMS`, `NOT_UNIQUE_ITEMS`, `WRONG_UUID`, `WRONG_IP`, `WRONG_CIDR`,
`WRONG_HOSTNAME`, `WRONG_BASE64`, `WRONG_HEX`, `WRONG_DIGEST`, `WRONG_MONGO_ID`,
`WRONG_HEX_COLOR`, `WRONG_MAC`, `WRONG_E164`, `WRONG_SEMVER`, `WRONG_JWT`,
`WRONG_CHECKSUM`, `WRONG_CREDIT_CARD_NUMBER`, `WRONG_IBAN`.

Override messages with `liver:custom_error/2`.

## See also

- [Comparing LIVR and standard](livr_vs_standard.md)
- [Standard rules](standard_rules.md)
- Upstream: [livr-spec.org](http://livr-spec.org)
