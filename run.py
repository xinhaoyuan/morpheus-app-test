#!/usr/bin/env python3

import argparse, sys, os
import subprocess as sp
import signal

parser = argparse.ArgumentParser()
parser.add_argument("-c", type = str, dest = "case", required = True)
parser.add_argument("-n", type = int, dest = "task_num", default = -10)
parser.add_argument("--repeat", type = int, dest = "repeat", default = 1000)
parser.add_argument("--sched", type = str, dest = "sched", required = True)
parser.add_argument("--pred", type = str, dest = "pred", default = "no")
parser.add_argument("--pred-skip", action = "store_true", dest = "pred_skip") 
args = parser.parse_args()

case_dir = {
    "locks-1" : "locks_test",
    "locks-2" : "locks_test",
    "gproc-1" : "gproc_test",
    "gproc-2" : "gproc_test",
    "gproc-3" : "gproc_test",
    "mnesia-1": "mnesia_test",
    "mnesia-2": "mnesia_test",
    "ms-1"    : "rabbit_test",
}

try:
    erl_path = sp.check_output("which erl", shell = True)
    sys.stderr.write("Use erl at {}\n".format(erl_path))
except sp.CalledProcessError:
    sys.stderr.write("Cannot find erl\n")
    sys.exit(1)

if args.case not in case_dir:
    sys.stderr.write("Unknown case {}.\n".format(args.case))
    sys.exit(1)

if args.task_num < 0:
    limit = -args.task_num;
    for t in range(1, limit + 1):
        lockfile = "lock-{case_dir}-{task_num}".format(case_dir = case_dir[args.case], task_num = t)
        try:
            os.open(lockfile, os.O_CREAT | os.O_EXCL)
            args.task_num = t
            break
        except FileExistsError:
            continue
    if args.task_num < 0:
        sys.stderr.write("Cannot find a idle task num\n")
        sys.exit(1)
    else:
        sys.stderr.write("Select task num {}\n".format(args.task_num))

filename = "output-{case}-{task_num}-{pred}-{repeat}-{sched}".format(
    case = args.case,
    task_num = args.task_num,
    repeat = args.repeat,
    sched = args.sched,
    pred = "{}_{}".format(args.pred, "s" if args.pred_skip else "w"))

if os.path.isfile(filename):
    sys.stderr.write("Output file [{}] already exists. Consider backing it up.\n".format(filename))
    os.remove(lockfile)
    sys.exit(1)

env = os.environ.copy()
env["SCHED"] = args.sched
env["PRED_SKIP"] = "t" if args.pred_skip else ""
env["PRED"] = args.pred
env["REPEAT"] = "1" # this is for the test's interal loop ...
p = sp.Popen(["./run-case.sh", filename, args.case, case_dir[args.case], str(args.task_num), str(args.repeat)], env = env)
try:
    p.wait()
except KeyboardInterrupt:
    sys.stderr.write("Terminating subprocess {}\n".format(p.pid))
    os.kill(p.pid, signal.SIGINT)

os.remove(lockfile)