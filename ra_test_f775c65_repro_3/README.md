This test reproduces the `badkey` error in ra.

This error happens when a `ra_server` in `follower` state receives an `append_entries_rpc` that override some log entries, which will call `pre_append_log_follower/2`.
For some reason `pre_append_log_follower/2` will try to revert the cluster configuration, which may not exist and could crash the server.

Note that the test will pass due to the resilience. By searching the output you will see the crash.
