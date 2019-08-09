#!/usr/bin/env python3

import sys, os, re

class Parser:

    def __init__(self):
        self.iteration_count = 0
        self.deque_count = []
        self.conc_length = []
        self.errors = {}

    def handle_input(self, input):
        for line in input:
            m = re.search("dequeue_count => ([0-9]*)", line)
            if m:
                self.deque_count.append(int(m.group(1)))
            m = re.search("conc_length => ([0-9]*)", line)
            if m:
                self.conc_length.append(int(m.group(1)))
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

if __name__ == "__main__":
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
    if len(parser.deque_count) > 0:
        sys.stdout.write("dequeue count: avg {:.02f}, max {}\n".format(avg(parser.deque_count), max(parser.deque_count)))
    if len(parser.conc_length) > 0:
        sys.stdout.write("conc_legnth: avg {:.02f}, max {}\n".format(avg(parser.conc_length), max(parser.conc_length)))
    sys.stdout.write("errors ({}):\n".format(len(parser.errors)))
    for k in parser.errors:
        sys.stdout.write("  {}: {}\n".format(k, parser.errors[k]))
