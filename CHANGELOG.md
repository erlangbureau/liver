# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `liver_json_schema`: bidirectional conversion between Liver field schemas and
  JSON Schema (Draft 2020-12). Import supports `rule_set => erlang_standard`
  (default) or `livr_spec`. See [doc/json_schema.md](doc/json_schema.md).
- LIVR aliased rules: `liver:add_aliased_rule/1` and the `aliased_rules`
  option of `validate/3`. See [README](README.md#add_aliased_rule1).
- The LIVR `aliases_positive` / `aliases_negative` suites run in
  `livr_rules_SUITE`.
- `livr_spec`: the rules added in
  [LIVR 2.5](https://github.com/koorchik/LIVR/blob/master/drafts/v2.5.md):
  `boolean`, `not_one_of`, `max_decimals`, `digits`, `starts_with`, `ends_with`, `contains`,
  `iso_datetime`, `iso_time`, `required_if`, `'when'`, `values`, `keys`, `any`,
  `list_length`, `unique`, `split`, `uuid`, `ip`, `cidr`, `hostname`, `base64`,
  `hex`, `digest`, `mongo_id`, `hex_color`, `mac`, `e164`, `semver`, `jwt`,
  `luhn`, `credit_card`, `iban`, and `min` / `max` options for `iso_date`.
  See [doc/livr_rules.md](doc/livr_rules.md#livr-25-rules).
- `liver:check_schema/1,2`: check a schema without data (unknown rules, wrong
  arguments of the 2.5 rules). `validate/2,3` run the same check, once per
  schema (the schemas that passed are cached), and raise `error(Reason)`,
  e.g. `error({invalid_args, Rule, Args})`.
- `liver_clock`: the current time of the relative date bounds (`today`,
  `yesterday`, `tomorrow`, in UTC); `liver_clock:set_time/1` fixes it in tests.
- The LIVR 2.5 suite (`test_suite_2.5`, with `schema_errors`) runs in
  `livr25_rules_SUITE`; `make livr-spec-import` imports it when the LIVR
  repository has it.

### Changed

- `validate/2,3` raise `error({unknown_rule, Rule})` for an unknown rule
  anywhere in the schema, also when the field is missing (LIVR 2.5). The
  field error `{unimplemented_rule, Rule}` (for a present field) and the
  silent `{ok, ...}` (for a missing one) are gone.
- CI: a rebar3 job runs `rebar3 ct` on OTP 25 and 29; `actions/checkout` v7.
- `livr_spec`: `eq` and `one_of` compare booleans as strings, so `true`
  equals `<<"true">>` (LIVR 2.5).
- `livr_spec`: `iso_date` returns `WRONG_DATE` for a number or a boolean
  (was `FORMAT_ERROR`), accepts only ASCII digits, and an argument that is
  not an options object is a schema error (LIVR 2.5).

### Fixed

- `livr_spec`: `equal_to_field` compares the values as strings
  (`<<"10">>` equals `10`), as in LIVR 2.0.
- `livr_spec`: `email` rejects whitespace (newlines, tabs) and a quoted local
  part with `@`, as the LIVR 2.0 pattern does.
- `livr_spec`: `'or'` runs on a missing field, so branches with `required`
  return `REQUIRED`.
- `like`: `$` matches only at the end of the value, as in JavaScript, so
  `^[0-9]{5}$` no longer accepts `"12345\n"`.
- `livr_spec`: rules skip `null` like `<<>>`, as in LIVR (they returned
  `FORMAT_ERROR`, `eq` and `one_of` `NOT_ALLOWED_VALUE`).
- `livr_spec`: `decimal` and `positive_decimal` accept integers (`10`), as
  in LIVR.
- `livr_spec`: `equal_to_field` inside `'or'` and aliased rules compares with
  the fields next to the validated one (it saw a made-up object), and inside
  `list_of` it sees no fields (LIVR 2.5).
- `livr_spec`: `list_of`, `list_of_objects` and `list_of_different_objects`
  return `FORMAT_ERROR` for an object given as a proplist.
- `livr_spec`: `url` accepts a port of 1–5 digits with a value of 0–65535
  only, so `https://example.com:65536` and `http://example.com:` return
  `WRONG_URL` (LIVR 2.5). A dot after the host and host labels longer than
  63 characters were already rejected.
- `return => proplist` with `nested_list`, `list_of`, `'or'` or a list as the
  root schema: it returned `format_error` or crashed.

## [1.1.0] - 2026-10-07

### Breaking

- **`erlang_standard` error codes are lowercase atoms** (`not_integer`,
  `required`, `format_error`) instead of LIVR-style binaries
  (`<<"NOT_INTEGER">>`). `livr_spec` still returns those binaries.

### Added

- `liver_openapi_schema`: export liver schemas to OpenAPI 3 documents and
  import Schema Objects into `erlang_standard` validation schemas (MVP).
  See [doc/openapi.md](doc/openapi.md).
- OpenAPI JSON I/O: OTP `json` on OTP 27+, **jsx** dependency on older OTP.
- [doc/livr_rules.md](doc/livr_rules.md) — full reference for the `livr_spec` rule set.

### Changed

- README and docs: Liver framed as **LIVR-inspired** with a full LIVR rule set,
  not as “incompatible with LIVR”. Implicit coercion explained in
  [doc/livr_vs_standard.md](doc/livr_vs_standard.md).
- [doc/standard_rules.md](doc/standard_rules.md) expanded as a standalone rule
  reference for `erlang_standard`.
- CI OTP matrix starts at **25** (`OTP-LATEST-25+`). OTP 24 is no longer
  published for `erlef/setup-beam` on aarch64 macOS runners.

## [1.0.0] - 2026-10-03

Almost ten years after the first version (**2017-11-21**), this is the stable
**1.0** that 0.9.x was always meant to become: an Erlang-oriented default rule
set, with full LIVR kept as an opt-in.

From the start Liver shipped LIVR-compatible validation and intentionally stayed
below 1.0 — a strict / Erlang-native rule set was planned but unfinished.
A partial `liver_strict_rules` surface appeared later in the 0.9 line; **1.0.0**
replaces that with `erlang_standard` (`liver_standard_rules`) and makes it the
default.

### Breaking

- **Default rule set is `erlang_standard`**, not LIVR names.
  Schemas that used LIVR rule names (`integer`, `string`, `nested_object`, …)
  without options will fail unless you opt in (see Migration below).
- **`liver_strict_rules` removed.** Its ideas live in `liver_standard_rules`
  (registered as `erlang_standard`).
- Application env key `rules` still overrides the active map when you do not
  pass `rule_set` / `livr_compatible`, but the built-in default is now
  `?ERLANG_STANDARD_RULES`.

### Migration from 0.9.x

Prefer one of:

```erlang
%% Opt into LIVR for a call
liver:validate(Schema, Data, #{rule_set => livr_spec}).

%% Alias
liver:validate(Schema, Data, #{livr_compatible => true}).
```

Or rewrite schemas to standard predicates (`is_integer`, `is_utf8_binary`,
`to_integer`, `nested_map`, …). See [doc/livr_vs_standard.md](doc/livr_vs_standard.md)
and [doc/standard_rules.md](doc/standard_rules.md).

### Added

- `rule_set` option: `erlang_standard` | `livr_spec` | named set |
  inline `#{Rule => Module}` | **ordered list** (first wins on name clash) |
  `{mixed, erlang_standard}` / `{mixed, livr_spec}` aliases.
- `liver:add_rule_set/2` to register named rule maps for composition.
- Broader `erlang_standard` rule surface (type predicates, converters,
  sizes, nested map/list/proplist).
- Internationalized `email` / `url` in the standard set (Unicode local-parts
  and IDN hosts, including Cyrillic IDN such as `.укр` / `.бел`).
- Local LIVR fixture suite under `tests/cases/livr/` (imported from upstream
  LIVR; maps + proplists).
- Coveralls coverage reporting in CI.

### Changed

- LIVR import no longer depends on **jsx**; uses OTP `json`.
- CI / dialyzer targets focused on modern OTP (24+ matrix where applicable).
- `erlang_standard` tests live in a readable CT suite (maps and proplists).
- Packaging: committed `ebin/` removed; ship `src/liver.app.src` so both
  **erlang.mk** and **rebar3** consumers build the `.app` locally.
- Application `vsn` comes from **git** (`git describe` / `{vsn, git}`): tag the
  release, no manual version edits in `Makefile` / `.app.src`.

### Removed

- `src/sets/liver_strict_rules.erl`.
- Checked-in `ebin/liver.app` (generated at build time).

## [0.9.4] - 2024-04-04

Still LIVR by default. Expands the optional strict / Erlang-oriented surface
that had been growing on the 0.9.x line, plus assorted fixes and CI updates.

### Added

- More strict rules (`is_*` predicates and related helpers) alongside LIVR.
- Positive / negative options for `is_integer`.
- Validation for non-map root values (lists and related cases).

### Fixed

- Root element datatype detection.
- `not_empty_list`, `is_char_binary`, email rule edge cases.

### Changed

- Travis → GitHub Actions workflow updates; broader documented OTP range.

## [0.9.3] - 2019-06-01

### Fixed

- Email rule.

## [0.9.2] - 2019-05-31

### Fixed

- Normalization for the `or` rule.

## [0.9.1] - 2018-06-01

### Changed

- Packaging / build tidy-ups (`Makefile`, `.app.src`).

## [0.9.0] - 2018-01-29

First git tag. The **first version** of the library is **2017-11-21**: a
lightweight Erlang validator **compatible with the LIVR specification** (maps
and proplists, custom rules, Unicode-aware modifiers, nested structures).

There was **no strict / Erlang-native rule set** yet — that gap is why the
version stayed at **0.9.x** until 1.0.0 could ship a complete default beyond
LIVR.

### Added

- LIVR rule set as the built-in default.
- Common Tests for maps and proplists; coveralls / Travis CI.
- Rules including `email`, `url`, `iso_date`, `equal_to_field`, `variable_object`,
  `or`, list meta-rules, and Unicode `to_lower` / `to_upper`.

[Unreleased]: https://github.com/erlangbureau/liver/compare/1.1.0...HEAD
[1.1.0]: https://github.com/erlangbureau/liver/compare/1.0.0...1.1.0
[1.0.0]: https://github.com/erlangbureau/liver/compare/0.9.4...1.0.0
[0.9.4]: https://github.com/erlangbureau/liver/compare/0.9.3...0.9.4
[0.9.3]: https://github.com/erlangbureau/liver/compare/0.9.2...0.9.3
[0.9.2]: https://github.com/erlangbureau/liver/compare/0.9.1...0.9.2
[0.9.1]: https://github.com/erlangbureau/liver/compare/0.9.0...0.9.1
[0.9.0]: https://github.com/erlangbureau/liver/releases/tag/0.9.0
