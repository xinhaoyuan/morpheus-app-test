Bug reproduction with artifical delays (using `timer:sleep/1`).

This bug is in gen_leader bootstrap.
The {ldr,'DOWN', ...} from node monitors could come untimely,
during which most nodes enter the normal operation loop while leaving a node stuck in election loop.
The node in election loop will ignore operation messages and stuck the global system.

three artificial delays are in the patch. (TODO is this the minimum?)
