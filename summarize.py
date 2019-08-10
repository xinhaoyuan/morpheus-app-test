#!/usr/bin/env python3

import sys, os, re

class Parser:

    def __init__(self):
        self.iteration_count = 0
        self.num_races = []
        self.pop_counter = []
        self.deque_count = []
        self.conc_length = []
        self.loc_fn = []
        self.loc_fp = []
        self.ploc_fn = []
        self.ploc_fp = []
        self.errors = {}

    def handle_input(self, input):
        for line in input:
            m = re.search("# of races: ([0-9]*)", line)
            if m:
                self.num_races.append(int(m.group(1)))
            m = re.search("INFO - ctl stop .* pop_counter = ([0-9]*)", line)
            if m:
                self.pop_counter.append(int(m.group(1)))
            m = re.search("dequeue_count => ([0-9]*)", line)
            if m:
                self.deque_count.append(int(m.group(1)))
            m = re.search("conc_length => ([0-9]*)", line)
            if m:
                self.conc_length.append(int(m.group(1)))
            m = re.search("Location prediction: FP ([0-9]*), FN ([0-9]*)", line)
            if m:
                self.loc_fp.append(int(m.group(1)))
                self.loc_fn.append(int(m.group(2)))
            m = re.search("Proc-location prediction: FP ([0-9]*), FN ([0-9]*)", line)
            if m:
                self.ploc_fp.append(int(m.group(1)))
                self.ploc_fn.append(int(m.group(2)))
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

    def print_stat(name, data):
        if len(data) > 0:
            sys.stdout.write("{}: #: {} avg {:.02f}, max {}\n".format(name, len(data), avg(data), max(data)))

    sys.stdout.write("iterations: {}\n".format(parser.iteration_count))

    print_stat("loc fp", parser.loc_fp)
    print_stat("loc fn", parser.loc_fn)
    print_stat("ploc fp", parser.ploc_fp)
    print_stat("ploc fn", parser.ploc_fn)
    print_stat("num races", parser.num_races)
    print_stat("pop counter", parser.pop_counter)
    print_stat("deque count", parser.deque_count)
    print_stat("conc length", parser.conc_length)
    sys.stdout.write("errors ({}):\n".format(len(parser.errors)))
    for k in parser.errors:
        sys.stdout.write("  {}: {}\n".format(k, parser.errors[k]))
