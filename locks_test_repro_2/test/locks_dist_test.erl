-module(locks_dist_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 10, ?_test( test_entry() )}.

spawn_node(NodeName) ->
    {ok, Node} = slave:start("localhost", NodeName),
    ok = rpc:call(Node, code, add_paths, [code:get_path()]),
    Node.

test_entry() ->
    {ok, _} = net_kernel:start([node1@localhost, shortnames]),
    Node1 = node(),
    Node2 = spawn_node(node2),
    Node3 = spawn_node(node3),
    Nodes = [Node1, Node2, Node3],

    {[ok, ok], []} = rpc:multicall(Nodes -- [Node3], application, ensure_started, [locks]),

    Workers =
        lists:foldr(
          fun (Id, Acc) ->
                  Pid =
                      erlang:spawn(
                        lists:nth(Id, Nodes),
                        fun () ->
                                Self = self(),
                                receive Self -> ok end,
                                {ok, Agt} = locks_agent:start(),
                                locks:change_flag(Agt, abort_on_deadlock, true),
                                locks:change_flag(Agt, await_nodes, true),
                                LockOrder =
                                    case Id of
                                        1 -> [[2], [1]];
                                        2 -> [[1], [2]]
                                    end,
                                lists:foreach(
                                  fun (Lock) ->
                                          io:format("~p(~p) ~p lock ~p call ~p~n", [self(), Agt, Id, Lock, locks:lock(Agt, Lock, write, Nodes, all)])
                                  end, LockOrder)
                        end),
                  MRef = monitor(process, Pid),
                  Pid ! Pid,

                  [{Pid, MRef} | Acc]
          end, [], lists:seq(1, 2)),

    timer:sleep(250), io:format(user, "SP 1~n", []),
    rpc:call(Node3, application, ensure_started, [locks]),

    lists:foreach(
      fun ({_, MRef}) ->
              receive {'DOWN', MRef, _, _, _} -> ok end
      end, Workers),

    ok.
