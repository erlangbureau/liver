PROJECT = liver
PROJECT_DESCRIPTION = Lightweight Erlang validator based on LIVR specification
PROJECT_VERSION = 1.0.0

BUILD_DEPS = ci.erlang.mk
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-LATEST-24+
AUTO_CI_WINDOWS ?= OTP-LATEST-21+

CT_OPTS = -cover ./tests/cover.spec
TEST_DIR = tests
COVER=1

DIALYZER_OPTS += -I include

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master

# coveralls.mk prints DEP/PATCH while fetching; that pollutes `make ci-list`
# output used by ci.erlang.mk's GitHub Actions OTP matrix. Load it only
# outside the multi-OTP CI workflow (coverage job / local uploads).
ifndef CI_ERLANG_MK
DEP_PLUGINS = coveralls.mk
dep_coveralls.mk = git https://github.com/erlangbureau/coveralls.mk master
endif

include livr_spec.mk
include erlang.mk
