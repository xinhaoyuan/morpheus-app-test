#!/bin/bash

output="$1"
name="$2"
task_num="$3"
repeat="$4"
sched="$5"
pred="$6"

case $name in
    locks-1)
        case_dir=locks_test
        use_t=t
        testcase=locks_test
        ;;
    locks-2)
        case_dir=locks_test
        use_t=t
        testcase=locks_dist_test
        ;;
    gproc-1)
        case_dir=gproc_test
        use_t=f
        testcase=t_simple_reg
        ;;
    gproc-2)
        case_dir=gproc_test
        use_t=f
        testcase=t_simple_ensure_other
        ;;
    gproc-3)
        case_dir=gproc_test
        use_t=f
        testcase=t_master_dies
        ;;
    mnesia-1)
        case_dir=mnesia_test
        use_t=f
        testcase=add_copy_and_restart
        ;;
    mnesia-2)
        case_dir=mnesia_test
        use_t=f
        testcase=del_copy_and_restart
        ;;
    ms-1)
        case_dir=rabbit_test
        use_t=t
        testcase=mirrored_supervisor_test
        ;;
    *)
        echo "Unknown case name $name"
        exit 1
esac

lockfile="dir-lock-${case_dir}_copy_${task_num}"
if [ -e $lockfile ]; then
    echo "$lockfile exists. Other parallel test may be running. Be careful and remove the file to continue"
    exit 1
fi
touch "dir-lock-${case_dir}_copy_${task_num}"

./create_copies.sh $case_dir $task_num $task_num
    
for X in `seq 1 ${repeat}`; do
    if [ "$use_t" = t ]; then
        env REPEAT=1 PRED=$pred SCHED=$sched make -C ${case_dir}_copy_${task_num} t=$testcase | tee -a $output
    else
        env TESTCASE=$testcase REPEAT=1 PRED=$pred SCHED=$sched make -C ${case_dir}_copy_${task_num} t=$testcase | tee -a $output
    fi
done
rm $lockfile
