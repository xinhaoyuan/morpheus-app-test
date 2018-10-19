Use `keep_testing.sh` to find bugs.

There are two kinds of errors can be found.

 1. Deadlocks due to a node stuck in gen_leader election loop.

 2. Inconsistency view due to conflicted resolution in gproc_dist.erl.
