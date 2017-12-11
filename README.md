# Liver
[![Build Status](https://travis-ci.org/erlangbureau/liver.svg?branch=master)](https://travis-ci.org/erlangbureau/liver)
[![Coverage Status](https://coveralls.io/repos/github/erlangbureau/liver/badge.svg?branch=master)](https://coveralls.io/github/erlangbureau/liver?branch=master)

## DESCRIPTION
[![Logo](https://www.halstedsurgery.org/Upload/200710291014_3602_000.jpg)]()

Liver is a lightweight Erlang validator based on LIVR Specification (See http://livr-spec.org for details)

**Basic LIVR features:**

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

**Liver specific features:**
1. Support different data types (maps, proplists)
2. Strict Mode (returns errors on all fields that do not have validation rules)
3. Ability to return custom error codes
4. Additional set of strict rules (without implicit conversion of types)


## GETTING STARTED
1. Add as a dependency in your project:
  * For **rebar** add to rebar.config
  ```erl
{deps, [
    {liver, ".*",
        {git, "https://github.com/erlangbureau/liver.git", {branch, master}}
    }
]}.
```

  * For **erlang.mk** add to make file:
```erl
DEPS = liver
dep_liver = git https://github.com/erlangbureau/liver.git master
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

## USAGE
### Examples of data validation

Simple example:
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

More complex example:
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

Strict validation:
```erl
10> liver:validate(Schema, Input, [{strict, true}]).
{error,#{<<"address">> => #{<<"extra_field">> => <<"UNKNOWN_FIELD">>},
         <<"extra_field">> => <<"UNKNOWN_FIELD">>}}
```
