This is to demonstrate the unexpected member bug that a node remains in `member` even after `leave` is called.
The bug is due to a race condition between gossip messages and the restart of `partisan_default_peer_service_manager` (caused by the `leave` call).

To run this test, first run `./get_patched_partisan.sh` to get the modified partisan with necessary delay injections and debug messages (and also a `erlang.mk` build workaround).
Then run `make eunit` and you will see a test error in around 20s. (Yes, I need 20s to have a stable reproduction on my machine.)
