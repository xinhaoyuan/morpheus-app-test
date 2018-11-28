PROJECT = locks_test_repro_1
DEPS = locks
dep_locks = git https://github.com/uwiger/locks
dep_locks_commit = 8e9b2e33deec6ed375a38fc88f67eff792b91504

include ../erlang.mk

apps::
	@cd deps/locks; if git diff-index --quiet HEAD -- src/locks_watcher.erl; then echo "==== Patching locks with delays ===="; git apply ../../locks_delay.patch; fi

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
