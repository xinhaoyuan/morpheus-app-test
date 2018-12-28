#!/bin/sh

exec env ERL_COMPILER_OPTIONS=debug_info make eunit
