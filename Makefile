PROJECT = liver

TEST_DEPS = LIVR jsx

dep_jsx = git https://github.com/talentdeficit/jsx.git  v2.8.3
dep_LIVR = git https://github.com/koorchik/LIVR         master

TEST_DIR = tests

include erlang.mk
