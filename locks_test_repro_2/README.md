This test reproduces the deadlock in `locks_watcher`.

This bug was cause by a race condition of messages.
When there are two `locks_watcher` (say A and B) on the same node waiting for `locks_server`,
suppose A is the real watcher and B is running `another_watcher` (because it failed to register the locks_watcher name).
If A receives `locks_running` from the `locks_server` before `watch_me` from B, the `watch_me` message could be ignored since A will exit after receiving `locks_running`.
Thus B would never get any reply and become deadlock.

Here is the flow graph.
```
server                         watcher1                         watcher2
                                  |                                |
                               is_registered(server)? false [1]    |
                                  |                                |
                               register(watcher)? success [4]      |
                                  |                                |
                               wait for msg ...                    |
                                  |                                |
                                  |                             is_registered(server)? false [2]
                                  |                                |
register(server) [3]              |                                |
  |                               |                                |
send(watcher, locks_running) [5]  |                                |
  |                               |                             register(watcher)? failed [6]
  |                               |                                |
  |                               |                             send(watcher, watch_for_me) [7]
  |                               |                                |
  |                            recv locks_running                  |
  |                               |                                |
  |                            exit                                |
  |                                                                |
  |                                                             timeout
  |                                                                |
  |                                                             register(watcher)? true
  |                                                                |
  |                                                             wait for msg ...
---------------------------------DEADLOCK-----------------------------------
```

The conditions are:

  [1], [2] < [3]
  [4] < [6]
  [5] < [7]
