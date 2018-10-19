PROJECT = ra_test
DEPS = ra
dep_ra = git https://github.com/rabbitmq/ra.git
dep_ra_commit = f775c65fe14be1503dc33c0df14196dee603654f

include ../dep_morpheus.mk
include ../erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
