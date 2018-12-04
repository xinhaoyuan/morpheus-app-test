This test reproduces a deadlock in `gen_leader` with artifical delays (i.e. `timer:sleep/1`).

The deadlock is cause by delayed leader down messages `{ldr, 'DOWN', ...}`,
which make followers send requests to a dead leader.
There is no retry after receiving the delayed `{ldr, 'DOWN', ...}`.  
