#!/bin/sh

echo Test output redirected to /tmp/test_output.txt
i=0;
while (rm *@localhost 2>/dev/null; make eunit > /tmp/test_output.txt); do
    i=$((i + 1))
    echo Test $i
done
echo End on test $i
