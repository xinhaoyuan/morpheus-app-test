This test reproduces the deadlock in `locks_watcher`.

This bug was cause by multiple possible race conditions of messages.

1. watcher to watcher race.

```
server                         watcher1                         watcher2
                                  |                                |
                               is_registered(server)? false [1]    |
                                  |                                |
                               register(watcher)? success [2]      |
                                  |                                |
                               wait for msg ...                    |
                                  |                                |
                                  |                             is_registered(server)? false [3]
                                  |                                |
register(server) [4]              |                                |
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

  [1], [2], [3] < [4]
  
  [2] < [6]
  
  [5] < [7]
  
2. server to watcher race. 

```
server                         watcher
                                  |
                               is_registered(server)? false [1]
                                  |
register(server) [2]              |
  |                               |
send(watcher, locks_running) [3]  |
  |                               |
  |                            register(watcher)? success [4]
  |                               |
  |                            wait for msg ...
---------------------------------DEADLOCK-----------------------------------
```

The conditions are:

  [1] < [2]
  
  [3] < [4] (and all other register(watcher) instance, if exist)
