# OpenAPI helpers

`liver_openapi_schema` converts between Liver validation schemas and OpenAPI 3
Schema Objects / documents. This is an MVP scaffold — not a full OpenAPI toolkit.

## Export (liver → OpenAPI)

Build an OpenAPI 3.0.3 document from a map of `Path => RequestSchema`:

```erlang
Paths = #{
    <<"/echo">> => #{
        name => [required, is_utf8_binary],
        age  => [is_integer]
    }
},
Doc = liver_openapi_schema:generate_schema(Paths, #{output => raw}),
Json = liver_openapi_schema:generate_schema(Paths, #{output => json}).
```

Or from a module that exports `liver_schema/0` returning the same path map:

```erlang
liver_openapi_schema:generate(my_api, #{output => file, filename => "openapi.json"}).
```

Each path is exported as **POST** with JSON request body. A default RPC-style
response schema (`status` ok/error) is used unless you pass
`response_schema` in options.

Supported rule names: **LIVR** (`string`, `integer`, `nested_object`, …) and
**erlang_standard** (`is_integer`, `nested_map`, `one_of_terms`, `range`, …).
Unknown rules raise `{error, {unknown_rule, Name}}`.

## Import (OpenAPI Schema → liver)

Turn an OpenAPI **Schema Object** (decoded map) into an **erlang_standard**
field schema for `liver:validate/2`:

```erlang
{ok, Schema} = liver_openapi_schema:from_openapi_schema(#{
    type => object,
    required => [<<"name">>],
    properties => #{
        <<"name">> => #{type => string},
        <<"age">> => #{type => integer, minimum => 0, maximum => 120}
    }
}),
liver:validate(Schema, #{<<"name">> => <<"bob">>, <<"age">> => 30}).
```

JSON binaries are accepted (`json:decode` first).

### Supported (MVP)

| OpenAPI | Liver (`erlang_standard`) |
|---------|---------------------------|
| `object` + `properties` / `required` | field map; nested → `nested_map` |
| `array` + `items` | `nested_list` |
| `string` | `is_utf8_binary` |
| `string` + `format` email/uri/url/date | `email` / `url` / `iso_date` |
| `string` + `enum` | `one_of_terms` |
| `string` + minLength/maxLength | `byte_size` (byte length) |
| `integer` / `number` | `is_integer` / `is_number` |
| both `minimum` and `maximum` | `range` |
| `boolean` | `is_boolean` |

### Not supported yet

`$ref`, `allOf`, `oneOf` / `anyOf`, `additionalProperties`, `pattern`,
document-level paths/methods/parameters, and single-sided numeric bounds.
`nullable` is ignored (null will not pass the mapped type checks).

These return `{error, {unsupported, Reason}}`.
