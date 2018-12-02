PROJECT = rabbit_test
DEPS = rabbit_common rabbitmq
dep_rabbit_common = git https://github.com/rabbitmq/rabbitmq-common.git
dep_rabbit_common_commit = v3.7.8
dep_rabbitmq = git https://github.com/rabbitmq/rabbitmq-server.git
dep_rabbitmq_commit = v3.7.8

include ../erlang.mk

apps::
	@cd deps/rabbit_common; if git diff-index --quiet HEAD -- src/mirrored_supervisor.erl; then echo "==== Patching mirrored_supervisor with delays ===="; git apply ../../mirrored_supervisor_delay.patch; fi

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
