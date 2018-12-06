#!/usr/bin/env python3

import os, sys
import argparse
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("-r", type = int, action = "store", dest = "repeat", default = 1000)
parser.add_argument("--hide", action = "store_true", dest = "hide")
parser.add_argument("--hide-true", action = "store_true", dest = "hide_true")
parser.add_argument("--hide-false", action = "store_true", dest = "hide_false")
parser.add_argument("--split-output", action = "store_true", dest = "split_output")
parser.add_argument("args", nargs = argparse.REMAINDER)
args = parser.parse_args()

succ_counter = 0
failed_counter = 0
if args.hide:
    out_f = open(os.devnull, "w")
    err_f = out_f

for i in range(0, args.repeat):
    succ = None
    try:
        if args.split_output:
            p = subprocess.Popen(args.args, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
        else:
            p = subprocess.Popen(args.args, stdout = subprocess.PIPE, stderr = subprocess.STDOUT)

        stdout, stderr = p.communicate()

        succ = p.returncode == 0
    except subprocess.CalledProcessError as x:
        sys.stdout.write("Got exception {}\n".format(x))
        succ = False

    if succ:
        succ_counter += 1
    else:
        failed_counter += 1

    sys.stdout.write("run {}: {} ({}/{})\n".format(i, succ, succ_counter, failed_counter))

    if not args.hide:
        if (succ and not args.hide_true) or (not succ and not args.hide_false):
            sys.stdout.write("==== OUTPUT ====\n")
            sys.stdout.buffer.write(stdout)
            if args.split_output:
                sys.stdout.write("\n==== STDERR ====\n")
                sys.stdout.buffer.write(stderr)
            sys.stdout.write("\n")

print(args)
