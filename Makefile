PROJECT = liver
PROJECT_DESCRIPTION = Lightweight Erlang validator based on LIVR specification
PROJECT_VERSION = 1.0.0

BUILD_DEPS = ci.erlang.mk
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-LATEST-24+
AUTO_CI_WINDOWS ?= OTP-LATEST-21+

TEST_DEPS = LIVR jsx iso8601
TEST_DIR = tests
COVER=1

DIALYZER_OPTS += -I include

dep_ci.erlang.mk    = git https://github.com/ninenines/ci.erlang.mk         master
dep_jsx             = git https://github.com/talentdeficit/jsx.git          v2.8.3
dep_LIVR            = git https://github.com/koorchik/LIVR                  master
dep_iso8601         = git https://github.com/erlsci/iso8601.git             1.3.4
dep_coveralls-erl   = git https://github.com/RoadRunnr/coveralls-erl.git    main


include erlang.mk
