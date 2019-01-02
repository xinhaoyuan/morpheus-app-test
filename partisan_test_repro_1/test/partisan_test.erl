-module(partisan_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.


spawn_node(NodeName) ->
    {ok, Node} = slave:start("localhost", NodeName),
    ok = rpc:call(Node, code, add_paths, [code:get_path()]),
    Node.

list_eqv(A, B) ->
    lists:usort(A) =:= lists:usort(B).

test_entry() ->
    {ok, _} = net_kernel:start([master@localhost, shortnames]),
    Master = node(),
    Node1 = spawn_node(node1),
    Node2 = spawn_node(node2),
    Node3 = spawn_node(node3),
    lists:foreach(
      fun (N) ->
              rpc:call(N, partisan_config, set, [tls, false]),
              % rpc:call(N, partisan_config, set, [tracing, true]),
              {ok, _} = rpc:call(N, application, ensure_all_started, [partisan])
      end, [Node1, Node2, Node3]),
    P =
        spawn(Node2,
              fun() ->
                      ok = partisan_peer_service:join(Node1),
                      receive setoff -> ok end,
                      timer:sleep(1000),
                      %% For some reason, removing the print will hide the bug. Strange
                      io:format(user, "!!! ~w is leaving the cluster~n", [node()]),
                      ok = partisan_peer_service:leave(node()),
                      %% This print won't show up as the connection of the node is broken during the leave call
                      io:format(user, "!!! ~w left the cluster~n", [node()]),
                      ok
              end),
    timer:sleep(10000),
    P ! setoff,
    io:format(user, "!!! ~w is joining the cluster~n", [Node3]),
    ok = rpc:call(Node3, partisan_peer_service, join, [Node1]),
    io:format(user, "!!! ~w joined the cluster~n", [Node3]),
    timer:sleep(10000),
    {ok, Members} = rpc:call(Node1, partisan_peer_service, members, []),
    case list_eqv(Members, [Node1, Node3]) of
        true -> ok;
        false ->
            io:format(user, "!!! unexpected membership: ~w~n", [Members]),
            ?assert(false)
    end,
    ok.
