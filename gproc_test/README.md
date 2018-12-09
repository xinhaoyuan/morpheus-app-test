Use `keep_testing.sh` to find bugs.

There are two kinds of errors can be found.

 1. Deadlocks due to a node stuck in gen_leader election loop.

 2. Inconsistency view due to conflicted resolution in gproc_dist.erl.

# Env variables

`REPEAT` - how many times the test case is repeated.
`TESTCASE` - one of `t_master_dies`, `t_simple_reg`, or `t_simple_ensure_other`.
`SCHED` - scheduler name used in firedrill. It usually has one of `basicpos`, `pos`, or `rw`.
