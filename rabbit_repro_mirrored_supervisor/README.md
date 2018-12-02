This test reproduces a crash bug in `mirrored_supervior`.

When a sup join the mirrored group while another sup leaves, a race condition could cause `mirrored_supervisor:295` to crash due to invalid process access.

Two delays are injected to reproduce the error.
