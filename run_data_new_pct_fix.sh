#!/bin/sh

./run.py -c $1 --sched pct
./run.py -c $1 --sched pct      --pred ploc --pred-skip
