#!/bin/sh

OUTPUT_DIR=/tmp/out_$$
mkdir -p $OUTPUT_DIR
echo Output dir is $OUTPUT_DIR

i=0
passed=0
failed=0
while [ $i -lt 1000 ]; do
    i=$((i + 1))
    echo "Test $i ..."
    if (rm $OUTPUT_DIR/*@localhost 2>/dev/null; TEST_WD=$OUTPUT_DIR make eunit > $OUTPUT_DIR/output.txt); then
        echo "Passed"
        passed=$((passed + 1))
    else
        echo "Failed"
        failed=$((failed + 1))
    fi
done
echo "passed = $passed, failed = $failed"
rm -r $OUTPUT_DIR
