PROJECT = gproc_test_repro_3
DEPS = gproc gen_leader
dep_gen_leader = git https://github.com/garret-smith/gen_leader_revival.git
dep_gen_leader_commit = d9689e6e80cd8a437bc207d37cb53290ecd64b35
dep_gproc = git https://github.com/uwiger/gproc.git
dep_gproc_commit = ed37980aa895cd3643e101f36cc2c3bdf1daa7fc

.PHONY: default

default: all

include ../erlang.mk

apps::
	@cd deps/gen_leader; if git diff-index --quiet HEAD -- src/gen_leader.erl; then echo "==== Patching gen_leader with delays ===="; git apply ../../gen_leader_delay.patch; fi

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
