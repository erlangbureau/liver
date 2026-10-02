PROJECT = liver
PROJECT_DESCRIPTION = Lightweight Erlang validator based on LIVR specification
PROJECT_VERSION = 1.0.0

BUILD_DEPS = ci.erlang.mk
DEP_PLUGINS = coveralls.mk
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-LATEST-24+
AUTO_CI_WINDOWS ?= OTP-LATEST-21+

CT_OPTS = -cover ./tests/cover.spec
TEST_DEPS = coveralls.mk
TEST_DIR = tests
COVER=1

DIALYZER_OPTS += -I include

dep_ci.erlang.mk    = git https://github.com/ninenines/ci.erlang.mk         master
dep_coveralls.mk    = git https://github.com/erlangbureau/coveralls.mk      master

include livr_spec.mk
include erlang.mk
