PROJECT = locks_test_repro_1
DEPS = locks
dep_locks = git https://github.com/uwiger/locks
dep_locks_commit = 8e9b2e33deec6ed375a38fc88f67eff792b91504

include ../erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
