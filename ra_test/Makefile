PROJECT = ra_test
DEPS = gen_batch_server ra

dep_gen_batch_server = git https://github.com/rabbitmq/gen-batch-server.git
dep_gen_batch_server_commit = 68f9f607f89c18ec2048c7679cbaf5d925ae97c7

dep_ra = git https://github.com/rabbitmq/ra.git
dep_ra_commit = b6a0d626fe4e381fe27a40eef876cd17874db951

include ../dep_morpheus.mk
include ../erlang.mk

apps::
	@cd deps/ra; if git diff-index --quiet HEAD -- src/ra.erl; then echo "==== Patching ra with simplification ===="; git apply ../../ra_simplification.patch; fi

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
