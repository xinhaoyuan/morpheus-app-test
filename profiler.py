#!/usr/bin/env python3

import os, sys
import argparse
import subprocess
import tempfile

parser = argparse.ArgumentParser(
    description =
    "Repeatly run a command and collect result based on the return code (0: succ, 1: failed, other: unknown)"
)
parser.add_argument("-r", type = int, action = "store", dest = "repeat", default = 1000)
parser.add_argument("--hide", action = "store_true", dest = "hide")
parser.add_argument("--hide-true", action = "store_true", dest = "hide_true")
parser.add_argument("--hide-false", action = "store_true", dest = "hide_false")
parser.add_argument("--hide-unknown", action = "store_true", dest = "hide_unknown")
parser.add_argument("--refine-by", nargs = "+", action = "store", type = str, dest = "refine_by",
                    help =
                    "Command args prefix for refining the result. "
                    "When specified, the prefix appended with the return code will be executed, "
                    "where STDIN will be fed with STDOUT of the original command")
parser.add_argument("--split-output", action = "store_true", dest = "split_output")
parser.add_argument("args", nargs = argparse.REMAINDER)
args = parser.parse_args()

cmd = args.args
if len(cmd) > 0 and cmd[0] == "--":
    cmd = cmd[1:]
if len(cmd) == 0:
    sys.stderr.write("No command provided\n")
    sys.exit(1)

succ_counter = 0
failed_counter = 0
if args.hide:
    out_f = open(os.devnull, "w")
    err_f = out_f

for i in range(0, args.repeat):
    result = None
    try:
        if args.split_output:
            p = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
        else:
            p = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.STDOUT)

        stdout, stderr = p.communicate()

        result = p.returncode
    except subprocess.CalledProcessError as x:
        sys.stdout.write("Got exception {}\n".format(x))
        result = 2

    if args.refine_by is not None:
        refine_cmd = args.refine_by.copy()
        refine_cmd.append(str(result))
        try:
            with tempfile.NamedTemporaryFile() as tmp:
                tmp.file.write(stdout)
                tmp.file.flush()
                refiner = subprocess.Popen(refine_cmd, stdin = open(tmp.name))
                refiner.wait()
                result = refiner.returncode
        except subprocess.CalledProcessError as x:
            sys.stdout.write("Got exception in refiner: {}\n".format(x))
            result = 2

    if result == 0:
        succ_counter += 1
    elif result == 1:
        failed_counter += 1

    if not args.hide:
        if (result == 0 and not args.hide_true) \
           or (result == 1 and not args.hide_false) \
           or (not args.hide_unknown):
            sys.stdout.write("==== OUTPUT ====\n")
            sys.stdout.buffer.write(stdout)
            if args.split_output:
                sys.stdout.write("\n==== STDERR ====\n")
                sys.stdout.buffer.write(stderr)
            sys.stdout.write("\n")
    sys.stdout.write("Run {}: {} ({}/{})\n".format(
        i + 1,
        "success" if result == 0 else "failed" if result == 1 else "unknown",
        succ_counter, failed_counter))

print(args)
