#!/usr/bin/env python3

import argparse, sys, os
import subprocess as sp
import signal

parser = argparse.ArgumentParser()
parser.add_argument("-c", type = str, dest = "case", required = True)
parser.add_argument("-n", type = int, dest = "task_num", default = 1)
parser.add_argument("--repeat", type = int, dest = "repeat", default = 1000)
parser.add_argument("--sched", type = str, dest = "sched", required = True)
parser.add_argument("--pred", type = str, dest = "pred", default = "no")
args = parser.parse_args()

filename = "output-{case}-{task_num}-{repeat}-{sched}".format(
    case = args.case, task_num = args.task_num, repeat = args.repeat, sched = args.sched)

if os.path.isfile(filename):
    sys.stderr.write("Output file [{}] already exists. Consider backing it up.\n".format(filename))

p = sp.Popen(["./run-case.sh", filename, args.case, str(args.task_num), str(args.repeat), args.sched, args.pred])
try:
    p.wait()
except KeyboardInterrupt:
    sys.stderr.write("Terminating subprocess {}\n".format(p.pid))
    os.kill(p.pid, signal.SIGINT)
