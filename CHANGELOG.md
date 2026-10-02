# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `liver_openapi_schema`: export liver schemas to OpenAPI 3 documents and
  import Schema Objects into `erlang_standard` validation schemas (MVP).
  See [doc/openapi.md](doc/openapi.md).

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

[Unreleased]: https://github.com/erlangbureau/liver/compare/1.0.0...HEAD
[1.0.0]: https://github.com/erlangbureau/liver/compare/0.9.4...1.0.0
[0.9.4]: https://github.com/erlangbureau/liver/compare/0.9.3...0.9.4
[0.9.3]: https://github.com/erlangbureau/liver/compare/0.9.2...0.9.3
[0.9.2]: https://github.com/erlangbureau/liver/compare/0.9.1...0.9.2
[0.9.1]: https://github.com/erlangbureau/liver/compare/0.9.0...0.9.1
[0.9.0]: https://github.com/erlangbureau/liver/releases/tag/0.9.0
