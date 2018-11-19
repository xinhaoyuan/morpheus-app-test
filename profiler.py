#!/usr/bin/env python3

import os, sys
import argparse
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("-e", type = str, action = "append", dest = "env", default = [])
parser.add_argument("-r", type = int, action = "store", dest = "repeat", default = 1000)
parser.add_argument("--hide", action = "store_true", dest = "hide")
parser.add_argument("args", nargs = argparse.REMAINDER)
args = parser.parse_args()

env = os.environ.copy()
for item in args.env:
    tokens = str.split(item, "=")
    if len(tokens) != 2:
        continue
    env[tokens[0]] = tokens[1]

succ_counter = 0
failed_counter = 0
if args.hide:
    out_f = open(os.devnull, "w")
    err_f = out_f

for i in range(0, args.repeat):
    succ = None
    try:
        if args.hide:
            subprocess.check_call(args.args, env = env, stdout = out_f, stderr = err_f)
        else:
            subprocess.check_call(args.args, env = env)
        succ = True
    except subprocess.CalledProcessError as x:
        succ = False

    print("run {}: {}".format(i, succ))
    if succ:
        succ_counter += 1
    else:
        failed_counter += 1

print(args)
print("succ = {}, failed = {}".format(succ_counter, failed_counter))
