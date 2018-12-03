This test reproduces the deadlock in locks.

The root cause of the deadlock is that, `locks_agent:send_indirects` may send lock info to other agents that are not holding or acquiring the lock so far.
Suppose agent A sends lock info of [1] to agent B, with some version number x.
Later after no one is holding lock [1], `locks_server` will reset the version number of [1], but the lock info in B remains.
So later when B tries to lock [1], it will reject the lock info from the `locks_server`, since it has a lower version number than what is kept in B.
This leads to a deadlock, i.e. B will never be able to acquire [1].

Four critical delays are introduced in the testcase to reproduce the failure.
Four more delays in the beginning are just for having a determinisitic agent pid order, which according to symmetry are not critical.
