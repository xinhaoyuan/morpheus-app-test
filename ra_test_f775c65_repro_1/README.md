This test reproduces the `undefined_reply` error in ra.

This error happens when a `ra_server` in `await_condition` state (due to missing log entry) receives `request_vote` from a higher term.
The `ra_server` will buffer the request without the sender info, which is required later. When such information is missing, the node will crash.
This may not be critical due to the resilience guarantee of Raft protocol, but it weakens the guarantee by increasing crashes.

Note that the test will pass due to the resilience. By searching the output you will see the crash.
