#!/usr/bin/env python3

import sys, os, re

class Parser:

    def __init__(self):
        self.iteration_count = 0
        self.deque_count = []
        self.max_conc_length = []
        self.errors = {}

    def handle_input(self, input):
        for line in input:
            m = re.search("dequeue_count => ([0-9]*)", line)
            if m:
                self.deque_count.append(int(m.group(1)))
            m = re.search("max_conc_length => ([0-9]*)", line)
            if m:
                self.max_conc_length.append(int(m.group(1)))
            m = re.search("final state", line)
            if m:
                self.iteration_count += 1
                continue
            m = re.search("error:{badmatch,(.*)}", line)
            if m:
                error = m.group(1)
                if error not in self.errors:
                    self.errors[error] = 0
                self.errors[error] += 1

parser = Parser()

if len(sys.argv) == 1:
    parser.handle_input(sys.stdin)
else:
    for fn in sys.argv[1:]:
        with open(fn) as f:
            parser.handle_input(f)

def avg(l):
    return sum(l) / float(len(l))

sys.stdout.write("iterations: {}\n".format(parser.iteration_count))
sys.stdout.write("dequeue count: avg {:.02f}\n".format(avg(parser.deque_count))) 
sys.stdout.write("max_conc_legnth: avg {:.02f}, max {}\n".format(avg(parser.max_conc_length), max(parser.max_conc_length))) 
sys.stdout.write("errors ({}):\n".format(len(parser.errors)))
for k in parser.errors:
    sys.stdout.write("  {}: {}\n".format(k, parser.errors[k]))
