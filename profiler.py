#!/usr/bin/env python3

import os, sys
import argparse
import subprocess
import tempfile
import psutil

def kill(proc_pid):
        process = psutil.Process(proc_pid)
        for proc in process.children(recursive=True):
            try:
                proc.kill()
            except Exception as x:
                pass
        process.kill()

parser = argparse.ArgumentParser(
    description =
    "Repeatly run a command and collect result based on the return code - 0: success, 1: failed, other: unknown."
)
parser.add_argument("-r", type = int, action = "store", dest = "repeat", default = 1000,
                    help = "number of time to repeat")
parser.add_argument("--hide", action = "store_true", dest = "hide",
                    help = "to hide all output")
parser.add_argument("--hide-true", action = "store_true", dest = "hide_true",
                    help = "to hide output when the result is success (i.e. 0)")
parser.add_argument("--hide-false", action = "store_true", dest = "hide_false",
                    help = "hide output when the result is failed (i.e. 1)")
parser.add_argument("--hide-unknown", action = "store_true", dest = "hide_unknown",
                    help = "to hide output when the result is unknown")
parser.add_argument("--refine-by", nargs = "+", action = "store", type = str, dest = "refine_by",
                    help =
                    "command args prefix for refining the result. "
                    "When specified, the prefix appended with the return code will be executed, "
                    "where STDIN will be fed with STDOUT of the original command")
parser.add_argument("--timeout", type = int, dest = "timeout")
parser.add_argument("--split-output", action = "store_true", dest = "split_output",
                    help = "to split stdout and stderr")
parser.add_argument("command_args", nargs = argparse.REMAINDER)
args = parser.parse_args()

cmd = args.command_args
if len(cmd) > 0 and cmd[0] == "--":
    cmd = cmd[1:]
if len(cmd) == 0:
    sys.stderr.write("No command provided\n")
    sys.exit(1)

succ_counter = 0
failed_counter = 0

for i in range(0, args.repeat):
    result = None
    may_refine = args.refine_by is not None
    try:
        if args.split_output:
            p = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE, preexec_fn = os.setsid)
        else:
            p = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.STDOUT, preexec_fn = os.setsid)

        stdout, stderr = p.communicate(timeout = args.timeout)

        if may_refine:
            result = p.returncode
        else:
            result = 0 if p.returncode == 0 else 1
    except subprocess.CalledProcessError as x:
        sys.stdout.write("Got exception: {}\n".format(x))
        stdout = stderr = b""
        result = 2
        may_refine = False
        kill(p.pid)
        p.wait()
    except subprocess.TimeoutExpired as x:
        sys.stdout.write("Got timeout: {}\n".format(x))
        stdout = stderr = b""
        result = 2
        may_refine = False
        kill(p.pid)
        p.wait()

    assert(p.poll() is not None)

    if may_refine:
        refine_cmd = args.refine_by.copy()
        for i in range(0, len(refine_cmd)):
            if refine_cmd[i] == "%":
                refine_cmd[i] = str(result)
                break

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
