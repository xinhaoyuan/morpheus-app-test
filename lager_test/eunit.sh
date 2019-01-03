#!/bin/sh

if [ -z "$1" ]; then
    echo "Need test case"
    exit 1
fi
exec env ERL_COMPILER_OPTIONS=debug_info make eunit t=$1
