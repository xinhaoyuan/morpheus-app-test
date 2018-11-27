Bug reproduction with artifical delays (using `timer:sleep/1`).

This bug is in gen_leader bootstrap and gproc_dist.
In the testcase, there is 3 nodes.
Under certain interleaving (e.g. with the delays injected),
gen_leader may isolate node2, and let node1 and node3 handle the gproc request of reg_other(P) where P is a process in node2.
Later when node2 resumes, it assumes itself has the correct registry data for all process under node2, thus remove P from the registry.
Causing request loss and inconsistency.
