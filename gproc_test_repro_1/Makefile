PROJECT = gproc_test_repro_1
DEPS = gproc gen_leader
dep_gen_leader = git https://github.com/garret-smith/gen_leader_revival.git

.PHONY: default

default: all

include ../erlang.mk

apps::
	@cd deps/gen_leader; if git diff-index --quiet HEAD -- src/gen_leader.erl; then echo "==== Patching gen_leader with delays ===="; git apply ../../gen_leader_delay.patch; fi

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
