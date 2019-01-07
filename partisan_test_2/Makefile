PROJECT = partisan_test
DEPS = types uuid time_compat rand_compat lager partisan

dep_types = hex 0.1.8
dep_uuid = git https://github.com/okeuday/uuid.git
dep_uuid_commit = fd20692d4fd6b72db029208dbc0e1a41b35de641
dep_time_compat = hex 0.0.1
dep_rand_compat = hex 0.0.3
dep_lager = hex 3.6.8
# dep_partisan = git https://github.com/lasp-lang/partisan.git
# dep_partisan_commit = dcf5d8908440bc5b0be41f669bb9f8f407ba6cd3
dep_partisan = cp patched/partisan

.PHONY: default

default: all

deps :: patched/partisan

patched/partisan:
	./get_patched_partisan.sh

include ../dep_morpheus.mk
include ../erlang.mk
