#!/usr/bin/env python3

import sys, os, re

iteration_count = 0
deque_count = []
for line in sys.stdin:
    m = re.search("dequeue_count => ([0-9]*)", line)
    if m:
        deque_count.append(int(m.group(1)))
        continue
    m = re.search("final state", line)
    if m:
        iteration_count += 1

def avg(l):
    return sum(l) / float(len(l))

sys.stdout.write("iterations: {}\n".format(iteration_count))
sys.stdout.write("dequeue count: avg {:.02f}\n".format(avg(deque_count))) 
