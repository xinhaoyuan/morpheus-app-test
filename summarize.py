#!/usr/bin/env python3

import sys, os, re

iteration_count = 0
deque_count = []
max_conc_length = []
for line in sys.stdin:
    m = re.search("dequeue_count => ([0-9]*)", line)
    if m:
        deque_count.append(int(m.group(1)))
    m = re.search("max_conc_length => ([0-9]*)", line)
    if m:
        max_conc_length.append(int(m.group(1)))
    m = re.search("final state", line)
    if m:
        iteration_count += 1

def avg(l):
    return sum(l) / float(len(l))

sys.stdout.write("iterations: {}\n".format(iteration_count))
sys.stdout.write("dequeue count: avg {:.02f}\n".format(avg(deque_count))) 
sys.stdout.write("max_conc_legnth: avg {:.02f}, max {}\n".format(avg(max_conc_length), max(max_conc_length))) 
