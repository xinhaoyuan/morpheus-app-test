#!/bin/bash

output="$1"
name="$2"
case_dir="$3"
task_num="$4"
repeat="$5"

case $name in
    locks-1)
        use_t=t
        testcase=locks_test
        ;;
    locks-2)
        use_t=t
        testcase=locks_dist_test
        ;;
    gproc-1)
        use_t=f
        testcase=t_simple_reg
        ;;
    gproc-2)
        use_t=f
        testcase=t_simple_ensure_other
        ;;
    gproc-3)
        use_t=f
        testcase=t_master_dies
        ;;
    mnesia-1)
        use_t=f
        testcase=add_copy_and_restart
        ;;
    mnesia-2)
        use_t=f
        testcase=del_copy_and_restart
        ;;
    ms-1)
        use_t=t
        testcase=mirrored_supervisor_test
        ;;
    *)
        echo "Unknown case name $name"
        exit 1
esac

./create_copies.sh $case_dir $task_num $task_num
truncate -s 0 $output
echo Redirect output to $output
    
for X in `seq 1 ${repeat}`; do
    if [ "$use_t" = t ]; then
        make -C ${case_dir}_copy_${task_num} eunit t=$testcase >>$output 2>&1
    else
        TESTCASE=$testcase make -C ${case_dir}_copy_${task_num} eunit t=$testcase >>$output 2>&1
    fi
done
