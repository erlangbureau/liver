# Liver
[![Build Status](https://travis-ci.org/erlangbureau/liver.svg?branch=master)](https://travis-ci.org/erlangbureau/liver)
[![Coverage Status](https://coveralls.io/repos/github/erlangbureau/liver/badge.svg?branch=master)](https://coveralls.io/github/erlangbureau/liver?branch=master)

## Summary
[![Logo](https://upload.wikimedia.org/wikipedia/commons/d/d4/Liver.svg)]()

Liver is a lightweight Erlang validator based on LIVR Specification (See http://livr-spec.org for details)

## Table of Contents
* [Description](#description)
* [Geting Started](#geting-started)
* [Usage Examples](#usage-examples)
* [Exports](#exports)
* [License](#license)

## <a name='description'></a>Description
**LIVR specification features:**

1. Rules are declarative and language independent
2. Any number of rules for each field
3. Validator should return together errors for all fields
4. Exclude all fields that do not have validation rules described
5. Possibility to validate complex hierarchical structures
6. Easy to describe and understand validation
7. Returns understandable error codes (neither error messages nor numeric codes)
8. Easy to implement own rules (usually you will have several in every project)
9. Rules should be able to change results output ("trim", "nested_object", for example)
10. Multipurpose (user input validation, configs validation, contracts programming etc)
11. Unicode support

**This implementation specific features:**
1. Strict Mode (returns errors on all fields that do not have validation rules)
2. Support on-the-fly conversion from proplist to map and vice versa
3. Support list as root element, what is missing in the original specification
4. Support on-the-fly conversion keys from string to atom
5. Ability to return custom error codes
6. Additional set of strict rules (without implicit conversion of types)



## <a name='geting-started'></a>Geting Started
1. Add as a dependency in your project:
  * For **rebar** add to rebar.config
  ```erl
{deps, [
    {liver, ".*",
        {git, "https://github.com/erlangbureau/liver.git", {branch, "0.9.0"}}
    }
]}.
```

  * For **erlang.mk** add to make file:
```erl
DEPS = liver
dep_liver = git https://github.com/erlangbureau/liver.git 0.9.0
```

2. Add liver in **your_project.app.src** file in tuple **applications**:
```erl
    {applications, [
        kernel,
        stdlib,
        liver
    ]},
```
3. Thats all, now you can validate data, register your own rules or add aliases for built-in rules.

## <a name='usage-examples'></a>Usage Examples

Example of simple validation for object (map or proplist):
```erl
1> Schema1 = [{<<"first_name">>,[{length_between,[4,6]}]}].

2> Input1 =  [{<<"first_name">>,<<"Vasya">>}].

3> liver:validate(Schema1, Input1).
{ok, [{<<"first_name">>,<<"Vasya">>}]}

4> Schema2 = [{<<"number1">>,integer}].

5> Input2 =  [{<<"number1">>,-1.12}].

6> liver:validate(Schema2, Input2).
{error,[{<<"number1">>,<<"NOT_INTEGER">>}]}
```

Example of simple validation for list:
```erl
1> Schema = [{nested_object, #{pool_name => [required, is_atom]}}].

2> liver:validate(Schema, [[{pool_name, test1}], [{pool_name, test2}]], #{return => map}).
{ok,[#{pool_name => test1},#{pool_name => test2}]}

3> liver:validate(Schema, [[{pool_name, test1}], [{pool_nam, test2}]], #{return => map}).
{error,[null,#{pool_name => <<"REQUIRED">>}]}
```

Example of more complex validation for object (map or proplist):
```erl
7> Schema = #{
    <<"address">> => [required, {nested_object, #{
        <<"country">> => [required,{one_of,[[<<"Ukraine">>,<<"USA">>]]}],
        <<"zip">> => positive_integer,
        <<"street">> => required,
        <<"building">> => [required,positive_integer]
    }}]
}.

8> Input = #{
    <<"address">> => #{
        <<"country">> => <<"Ukraine">>,
        <<"zip">> => <<"12345">>,
        <<"street">> => <<"10">>,
        <<"building">> => <<"10">>,
        <<"extra_field">> => <<"will be removed">>
    },
    <<"extra_field">> => <<"will be removed">>
}.

9> liver:validate(Schema, Input).
{ok,#{<<"address">> => #{<<"building">> => 10,
        <<"country">> => <<"Ukraine">>,
        <<"street">> => <<"10">>,
        <<"zip">> => 12345}}}
```

Example of strict validation (fields that are not specified in the scheme are forbidden):
```erl
10> liver:validate(Schema, Input, [{strict, true}]).
{error,#{<<"address">> => #{<<"extra_field">> => <<"UNKNOWN_FIELD">>},
         <<"extra_field">> => <<"UNKNOWN_FIELD">>}}
```
## <a name='exports'></a>Exports

### `validate/2`

```erlang
validate(Schema, Input) -> {ok, Output} | {error, ErrorList}

  Schema, Input, Output, ErrorList = proplist() | map()
```
  Equivalent to `validate(Schema, Input, []).`

### `validate/3`

```erlang
validate(Schema, Input, Opts) -> {ok, Output} | {error, ErrorList}

  Schema, Input, Output, ErrorList, Opts = proplist() | map()
```
Parameter Opts is a proplist or map that specifies return type and validation strictness. Default values are used for omitted options. This means that not specifying any options `([])` is the same as specifying `[{return, as_is}, {strict, false}]`.

`{return, ReturnType}`

If set to `as_is` the type of `Output` wiil be the same as type of `Input`. If set to `map` the `Output` will be map. If set to `proplist` the `Output` will be proplist. Defaults to `as_is`

`{strict, boolean()}`

If set to `false` deletes from `Input` all fields that not defined in `Schema`. Or if set to `true` and `Input` has fields that not defined in `Schema` returns error. Defaults to `false`

## <a name='license'></a>License

Liver is released under the MIT License
