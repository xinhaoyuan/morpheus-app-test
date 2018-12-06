PROJECT = rabbit_test
DEPS = rabbit
dep_rabbit = git https://github.com/rabbitmq/rabbitmq-server.git
dep_rabbit_commit = v3.7.8

include ../dep_morpheus.mk
include ../erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
