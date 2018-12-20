PROJECT = gproc_test
DEPS = gproc gen_leader

dep_gproc = git https://github.com/uwiger/gproc
dep_gproc_commit = ed37980aa895cd3643e101f36cc2c3bdf1daa7fc
dep_gen_leader = git https://github.com/garret-smith/gen_leader_revival.git
dep_gen_leader_commit = d9689e6e80cd8a437bc207d37cb53290ecd64b35

include ../dep_morpheus.mk
include ../erlang.mk

apps::
	@cd deps/gen_leader; if git diff-index --quiet HEAD -- src/gen_leader.erl; then echo "==== Patching gen_leader with state exposure ===="; git apply ../../gen_leader_state.patch; fi

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
