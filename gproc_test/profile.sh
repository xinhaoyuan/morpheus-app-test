#!/bin/sh

i=0
passed=0
failed=0
while [ $i -lt 1000 ]; do
    i=$((i + 1))
    echo "Test $i ..."
    if (rm *@localhost 2>/dev/null; make eunit > /tmp/test_output.txt); then
        echo "Passed"
        passed=$((passed + 1))
    else
        echo "Failed"
        failed=$((failed + 1))
    fi
done
echo "passed = $passed, failed = $failed"
