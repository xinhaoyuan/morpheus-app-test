## Local test

`make eunit t=locks_test`

There is a deadlock due to inproperly ignored lock info.

## Dist test

`make eunit t=locks_dist_test`

There are deadlocks due to race conditions in locks_watcher, which makes a node waiting for another node forever.


