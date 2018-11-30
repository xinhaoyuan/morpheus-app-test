This test reproduces the `badmatch` error in ra.

This error happens when a `ra_server` in `leader` state (due to missing log entry) receives an `append_entries_reply` (with success = false) from another server that is not in the current cluster configuration.
This could be because the server sending `append_entries_reply` was in cluster in a previous configuration.

Note that the test will pass due to the resilience. By searching the output you will see the crash.
