#!/usr/bin/env python3

import os, sys, re
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-i", type = str, action = "store", dest = "input", required = True)
parser.add_argument("-a", type = int, action = "store", dest = "age", required = True)
parser.add_argument("-e", type = str, action = "store", dest = "match", required = False)
args = parser.parse_args()

def extract_info(lines):
    m = re.match("^{schedule,.*", lines[0])
    if not m:
        return None
    ret = {}
    for line in lines:
        m = re.match(".*age => ([0-9]+).*", line)
        if m:
            ret["age"] = int(m.group(1))
        m = re.match(".*weight => (undefined|[0-9]+).*", line)
        if m:
            ret["weight"] = 1 if m.group(1) == "undefined" else int(m.group(1))
        m = re.match(".*reset => \\[head,([^]]+)\\].*", line)
        if m:
            ret["reset"] = [int(x) for x in m.group(1).split(",")]
    return ret

class LineStreamHandler:

    def __init__(self, args):
        self.in_trace = False
        self.requests = []
        self.filtered_requests = []
        self.line_num = 0
        self.line_buf = None
        self.args = args

    def handle_line_buf(self):
        if self.line_buf is not None:
            info = extract_info(self.line_buf)
            if info is not None:
                self.requests.append(info)
                match = "age" in info and info["age"] >= self.args.age
                lines = "\n".join(self.line_buf)
                if match and self.args.match is not None and not re.search(self.args.match, lines):
                    match = False
                if match:
                    s_reset = []
                    cur = len(self.requests) - 1;
                    for d in info["reset"]:
                        s = [0,0]
                        for i in range(0, d):
                            cur -= 1;
                            s[0 if self.requests[cur]["weight"] == 1 else 1] += 1;
                        s_reset.append(s)
                    info["s_reset"] = s_reset
                    self.filtered_requests.append((info, self.begin_line_num, lines))

    def handle_line(self, raw_line):
        line = raw_line.rstrip()
        self.line_num += 1
        if self.in_trace:
            if line == "TRACE END":
                self.handle_line_buf()
                self.in_trace = False
            if line[0] == ' ':
                self.line_buf.append(line)
            else:
                self.handle_line_buf()
                self.begin_line_num = self.line_num
                self.line_buf = [line]
        else:
            if line == "TRACE BEGIN":
                self.in_trace = True
                self.line_buf = None
                self.begin_line_num = self.line_num + 1
                self.requests = []
                self.filtered_requests = []
            elif line == "  Test passed.":
                self.requests = []
            elif line == "**error:{badmatch,deadlock}":
                for age, line_num, lines in self.filtered_requests:
                    sys.stdout.write("{},{}\n".format(age, line_num))
                    sys.stdout.write(lines)
                    sys.stdout.write("\n")
                sys.stdout.write("========\n")
                self.requests = []
                self.filtered_requests = []

handler = LineStreamHandler(args)
for line in open(args.input):
    handler.handle_line(line)
