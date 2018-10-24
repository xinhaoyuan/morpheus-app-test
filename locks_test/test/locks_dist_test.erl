-module(locks_dist_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

test_entry() ->
    {Ctl, MRef} = morpheus_sandbox:start(
                    ?MODULE, t_sandbox_entry, [],
                    [ monitor
                    , { fd_opts
                      , [ { scheduler
                          , {basicpos, []} }
                        , verbose_final ] }
                    , {node, node1@localhost}
                    , {clock_limit, 600000}
                    , {clock_offset, 1539105131938}
                    , stop_on_deadlock
                    %% , trace_send, trace_receive
                    %% , verbose_handle, verbose_ctl
                    %% , {trace_from_start, true}
                    ]),
    success = receive {'DOWN', MRef, _, _, Reason} -> Reason end,
    ok.

-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

t_sandbox_entry() ->
    Nodes = [Node1, Node2, Node3] =
        [node1@localhost, node2@localhost, node3@localhost],
    lists:foreach(fun (Node) ->
                          ?GH:bootstrap_remote(Node)
                  end, Nodes -- [Node1]),
    ?GH:bootstrap(Node1),
    ?G:set_flags([{tracing, true}]),

    {[ok, ok, ok], []} = rpc:multicall(Nodes, application, start, [locks]),

    ?GH:sync_task(
       [ repeat, 100
       , fun () ->
                 io:format(user, "Test start~n", []),
                 Workers =
                     lists:foldr(
                       fun (Id, Acc) ->
                               {Pid, MRef} =
                                   erlang:spawn_monitor(
                                     lists:nth(Id, Nodes),
                                     fun () ->
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
                               [{Pid, MRef} | Acc]
                       end, [], lists:seq(1, 2)),

                 ok = rpc:call(Node3, application, stop, [locks]),
                 ok = rpc:call(Node3, application, start, [locks]),

                 lists:foreach(
                   fun ({_, MRef}) ->
                           receive {'DOWN', MRef, _, _, _} -> ok end
                   end, Workers),
                 ok
         end
       ]),

    ?G:exit_with(success).
