#!/bin/sh

./run.py -c $1 --sched pct
./run.py -c $1 --sched pct      --pred ploc --pred-skip
./run.py -c $1 --sched rw       --pred ploc --pred-skip
./run.py -c $1 --sched basicpos --pred ploc --pred-skip
./run.py -c $1 --sched pos      --pred ploc --pred-skip
./run.py -c $1 --sched rapos
./run.py -c $1 --sched rapos    --pred ploc --pred-skip
