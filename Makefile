PROJECT = liver
PROJECT_DESCRIPTION = Lightweight Erlang validator based on LIVR specification
PROJECT_VERSION = 0.9.2

TEST_DEPS = LIVR jsx iso8601

dep_jsx         = git https://github.com/talentdeficit/jsx.git  v2.8.3
dep_LIVR        = git https://github.com/koorchik/LIVR          master
dep_iso8601     = git https://github.com/erlsci/iso8601.git     1.3
dep_ecoveralls  = git https://github.com/nifoc/ecoveralls       master

ifeq ($(USER),travis)
	TEST_DEPS += ecoveralls
endif

TEST_DIR = tests
CT_OPTS = -cover ./tests/cover.spec

include erlang.mk

coverage-report: $(shell ls -1rt `find logs -type f -name \*.coverdata 2>/dev/null` | tail -n1)
	$(gen_verbose) erl -noshell -pa ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()'

.PHONY: coverage-report
