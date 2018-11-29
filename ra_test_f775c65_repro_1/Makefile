PROJECT = ra_test_f775c65_repro_1
DEPS = ra
dep_ra = git https://github.com/rabbitmq/ra.git
dep_ra_commit = f775c65fe14be1503dc33c0df14196dee603654f

include ../erlang.mk

apps::
	@cd deps/ra; if git diff-index --quiet HEAD -- src/ra_server.erl; then echo "==== Patching ra with delays ===="; git apply ../../ra_delay.patch; fi

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
