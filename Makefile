PROJECT = liver
PROJECT_DESCRIPTION = Lightweight Erlang validator (erlang_standard default, LIVR opt-in)
# From nearest git tag (override freely: make PROJECT_VERSION=1.0.0).
PROJECT_VERSION ?= $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || git describe --dirty --abbrev=7 --tags --always 2>/dev/null || echo 0.0.0)

BUILD_DEPS = ci.erlang.mk
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-LATEST-25+
AUTO_CI_WINDOWS ?= OTP-LATEST-25+

CT_OPTS = -cover ./tests/cover.spec
TEST_DIR = tests
COVER=1

DIALYZER_OPTS += -I include

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master

# OpenAPI JSON: OTP 27+ has `json`; older releases use jsx.
OTP_RELEASE := $(shell erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().')
ifeq ($(shell test "$(OTP_RELEASE)" -lt 27 >/dev/null 2>&1; echo $$?),0)
DEPS += jsx
dep_jsx = hex 3.1.0
endif

# coveralls.mk prints DEP/PATCH while fetching; that pollutes `make ci-list`
# output used by ci.erlang.mk's GitHub Actions OTP matrix. Load it only
# outside the multi-OTP CI workflow (coverage job / local uploads).
ifndef CI_ERLANG_MK
BUILD_DEPS += coveralls.mk
DEP_PLUGINS = coveralls.mk
dep_coveralls.mk = git https://github.com/erlangbureau/coveralls.mk master
endif

include livr_spec.mk
include erlang.mk

# rebar3 resolves `{vsn, git}` itself; erlang.mk copies .app.src as-is, so
# expand it here for OTP-valid ebin/$(PROJECT).app.
ebin/$(PROJECT).app::
	$(verbose) if grep -Eq '\{vsn,[[:space:]]*git\}' '$@'; then \
		sed -e 's/{vsn,[[:space:]]*git}/{vsn, "$(PROJECT_VERSION)"}/' '$@' > '$@.tmp' \
		&& mv '$@.tmp' '$@'; \
	fi
