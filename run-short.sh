#!/bin/sh

rm $1/acc.dat
for i in `seq 1 100`; do
    if [ -z "$USE_T" ]; then
        env PRED=ploc TESTCASE=$2 REPEAT=1 SCHED=basicpos make -C $1 eunit | tee -a output-ploc-${2}-simrw.txt
    else
        env PRED=ploc REPEAT=1 SCHED=basicpos make -C $1 eunit t=$2 | tee -a output-ploc-${2}-simrw.txt
    fi
done
