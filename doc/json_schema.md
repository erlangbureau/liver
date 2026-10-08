# JSON Schema helpers

`liver_json_schema` converts between Liver validation schemas and
[JSON Schema](https://json-schema.org/) documents (Draft 2020-12 by default).

This is an MVP — not a full JSON Schema toolkit. OpenAPI Schema Objects share
much of the same vocabulary; see also [openapi.md](openapi.md).

## Export (liver → JSON Schema)

Pass a **field schema** (same shape as `liver:validate/2`) or a **rule list**:

```erlang
JS = liver_json_schema:to_json_schema(#{
    name => [required, is_utf8_binary],
    age  => [is_integer]
}),
%% => #{<<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
%%     type => object, properties => …, required => [name]}

Frag = liver_json_schema:to_json_schema([email, {min_length, 3}]),
Bin  = liver_json_schema:to_json_schema(Schema, #{output => json}).
```

| Option | Default | Description |
|--------|---------|-------------|
| `output` | `raw` | `raw` \| `json` \| `file` |
| `filename` | `"schema.json"` | Used when `output => file` |
| `include_schema` | `true` | Add `"$schema"` |
| `schema_uri` | Draft 2020-12 URI | Override dialect URI |

Rule mapping is shared with `liver_openapi_schema` (LIVR and `erlang_standard`
names). OpenAPI-only `nullable` is rewritten to a `type` union with `null`.

## Import (JSON Schema → liver)

Top-level Schema must be an **object** (with `properties` / `required`).
Choose the Liver dialect with `rule_set`:

```erlang
{ok, Std} = liver_json_schema:from_json_schema(JsonSchema),
%% erlang_standard (default)

{ok, Livr} = liver_json_schema:from_json_schema(JsonSchema, #{
    rule_set => livr_spec
}),
liver:validate(Livr, Data, #{rule_set => livr_spec}).
```

JSON binaries are accepted. Encoding uses OTP `json` on OTP 27+ and **jsx**
on older releases.

### Mapping (MVP)

| JSON Schema | `erlang_standard` | `livr_spec` |
|-------------|-------------------|-------------|
| `object` + `properties` / `required` | field map; nested → `nested_map` | nested → `nested_object` |
| `array` + `items` | `nested_list` | `list_of` |
| `string` | `is_utf8_binary` | `string` |
| `string` + `format` email/uri/url/date | `email` / `url` / `iso_date` | same |
| `string` + `pattern` | unsupported | `like` |
| `string` + minLength/maxLength | `byte_size` | `min_length` / `max_length` / … |
| `integer` / `number` | `is_integer` / `is_number` | `integer` / `decimal` |
| both `minimum` and `maximum` | `range` | `number_between` |
| one-sided min/max | skipped | `min_number` / `max_number` |
| `boolean` | `is_boolean` | `one_of` `[true, false]` |
| `enum` | `one_of_terms` | `one_of` |
| `const` | `one_of_terms` | `eq` |
| `type: ["string","null"]` | non-null branch only | same |

### Not supported yet

`$ref`, `$defs` / `definitions`, `allOf`, `oneOf` / `anyOf`,
`additionalProperties` (other than `false`), multi-type unions beyond
nullability, and document-level composition.

These return `{error, {unsupported, Reason}}`.
