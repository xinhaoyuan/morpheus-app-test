-module(partisan_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

?MORPHEUS_CB_TO_OVERRIDE(_, gen_server, loop, 7) ->
    {true, callback};
%% partisan_mochiglobal uses dynamic model trick for global mapping,
%% which Morpheus doesn't support well ... workaround it
?MORPHEUS_CB_TO_OVERRIDE(_, partisan_mochiglobal, get, 2) ->
    {true, callback};
?MORPHEUS_CB_TO_OVERRIDE(_, partisan_mochiglobal, put, 2) ->
    {true, callback};
?MORPHEUS_CB_TO_OVERRIDE(_, partisan_mochiglobal, delete, 1) ->
    {true, callback};
?MORPHEUS_CB_TO_OVERRIDE(_, _, _, _) ->
    false.

?MORPHEUS_CB_HANDLE_OVERRIDE(_, gen_server, NewModule, loop, NewEntry, Args, _Ann) ->
    %% This is a bit of hacky to extract info from the arguments
    %% The reporting interface in callback is not stable yet ...
    [_, Name, State, Mod | _] = Args,
    case Mod of
        partisan_default_peer_service_manager ->
            Members = element(7, State),
            MembersList = sets:to_list(state_orset:query(Members)),
            io:format(user,
                      "!!! state @ ~w:~w ~p~n  ~p~n",
                      [self(), morpheus_sandbox:get_node(), MembersList, State]),
            ok;
        %% locks_agent seems to have its own loop ...
        %% locks_agent -> ok;
        _ -> ok
    end,
    %% forward to the original code
    apply(NewModule, NewEntry, Args);
%% Note that the callbacks are called in uninstrumented context.
?MORPHEUS_CB_HANDLE_OVERRIDE(_, partisan_mochiglobal, _, get, _, [Key, Default], _Ann) ->
    Node = morpheus_sandbox:get_node(),
    case ets:lookup(test_tab_mochiglobal, {Node, Key}) of
        [] -> Default;
        [{_, V}] -> V
    end;
?MORPHEUS_CB_HANDLE_OVERRIDE(_, partisan_mochiglobal, _, put, _, [Key, Value], _Ann) ->
    Node = morpheus_sandbox:get_node(),
    ets:delete(test_tab_mochiglobal, {Node, Key}),
    ets:insert(test_tab_mochiglobal, {{Node, Key}, Value});
?MORPHEUS_CB_HANDLE_OVERRIDE(_, partisan_mochiglobal, _, delete, _, [Key], _Ann) ->
    Node = morpheus_sandbox:get_node(),
    ets:delete(test_tab_mochiglobal, {Node, Key}).

-compile(export_all).

all_test_() ->
    {timeout, 3600, ?_test( test_entry() )}.

-define(S, morpheus_sandbox).
-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

-define(config(K, C), proplists:get_value(K, C)).

test_entry() ->
    Config = [ {repeat, 1}
             , {to_trace, true}
             , {testcase, both_join_and_leave}
             , {sched, basicpos}
             , {nodes, [master@localhost, node1@localhost, node2@localhost, node3@localhost]}
             ],
    test_tab_mochiglobal = ets:new(test_tab_mochiglobal, [named_table, public]),
    {Ctl, MRef} =
        ?S:start(
           ?MODULE, test_sandbox_entry, [Config],
           [ monitor
           , {fd_opts,
              [{scheduler,
                { ?config(sched, Config)
                , [ %% {seed, {exrop,[58554603788651093|137564435558459620]}}
                    %% {seed, {exrop,[50426418134840848|258071354043047857]}}
                    %% {seed, {exrop,[192331848488008564|14292699964984100]}}
                  ]
                }}]}
           , {heartbeat, once}
           , {clock_offset, 1538099922306}
           , {clock_limit, ?config(repeat, Config) * 30000 + 30000}
           , {node, master@localhost}
           , {undet_timeout, 250}
           , {aux_module, ?MODULE}
           , {aux_data, undefined}
           , stop_on_deadlock
           %% , trace_send, trace_receive
           %% , verbose_handle, verbose_ctl
           ]
          ),
    ?assertEqual(success, receive {'DOWN', MRef, _, _, Reason} -> Reason end),
    ok.

test_sandbox_entry(Config) ->
    Nodes = ?config(nodes, Config),
    ?GH:bootstrap(master@localhost),
    lists:foreach(
      fun (N) ->
              ?GH:bootstrap_remote(N)
      end, Nodes -- [node()]),
    lists:foreach(
      fun (N) ->
              rpc:call(N, partisan_config, set, [tls, false]),
              %% rpc:call(N, partisan_config, set, [partisan_peer_service_manager, partisan_client_server_peer_service_manager]),
              {ok, _} = rpc:call(N, application, ensure_all_started, [partisan])
      end, Nodes),
    test_tab = ets:new(test_tab, [named_table, public]),
    ets:insert(test_tab, {repeat_counter, 0}),
    ?GH:sync_task(
       [ repeat, ?config(repeat, Config)
       , fun () ->
                 RC = ets:update_counter(test_tab, repeat_counter, 1),
                 io:format(user, "Test ~w~n", [RC]),
                 case RC =:= ?config(repeat, Config) andalso ?config(to_trace, Config) of
                     true ->
                         ?G:set_flags([{tracing, true}]);
                     false ->
                         ok
                 end,
                 timer:sleep(100),
                 try
                     apply(?MODULE, ?config(testcase, Config), [Config])
                 catch
                     C:R ->
                         ?G:exit_with({'catch', C, R})
                 end
         end ]),
    ?G:exit_with(success),
    ok.

list_eqv(A, B) ->
    lists:usort(A) =:= lists:usort(B).

chain_join_and_check(Config) ->
   ?GH:sync_task(
       [ par
       , fun () ->
                 N = node1@localhost,
                 ok = rpc:call(N, partisan_peer_service, join, [master@localhost])
         end
       , fun () ->
                 N = node2@localhost,
                 ok = rpc:call(N, partisan_peer_service, join, [node1@localhost])
         end
       , fun () ->
                 N = node3@localhost,
                 ok = rpc:call(N, partisan_peer_service, join, [node2@localhost])
         end
       ]),
    timer:sleep(15000),
    {ok, Members} = partisan_peer_service:members(),
    io:format(user, "!!! members ~p~n", [Members]),
    true = list_eqv(Members, ?config(nodes, Config)),
    lists:foreach(
      fun (N) ->
              ok = rpc:call(N, partisan_peer_service, leave, [N])
      end, ?config(nodes, Config) -- [node()]),
    ok.

both_join_and_leave(Config) ->
    N1 = rpc:call(node1@localhost, partisan_peer_service_manager, myself, []),
    N2 = rpc:call(node2@localhost, partisan_peer_service_manager, myself, []),
    N3 = rpc:call(node2@localhost, partisan_peer_service_manager, myself, []),
    ?GH:sync_task(
       [ par
       , fun () ->
                 N = node1@localhost,
                 ok = rpc:call(N, partisan_peer_service, join, [master@localhost]),
                 ok = rpc:call(N, partisan_peer_service, leave, [N1])
         end
       , fun () ->
                 N = node2@localhost,
                 ok = rpc:call(N, partisan_peer_service, join, [master@localhost]),
                 ok = rpc:call(N, partisan_peer_service, leave, [N2])
         end
       , fun () ->
                 N = node3@localhost,
                 ok = rpc:call(N, partisan_peer_service, join, [master@localhost])
         end
       ]),
    timer:sleep(15000),
    {ok, MasterMembers} = partisan_peer_service:members(),
    {ok, Node1Members} = rpc:call(node1@localhost, partisan_peer_service, members, []),
    {ok, Node2Members} = rpc:call(node2@localhost, partisan_peer_service, members, []),
    {ok, Node3Members} = rpc:call(node3@localhost, partisan_peer_service, members, []),
    io:format(user,
              "!!! view of master ~p~n"
              "    view of node1 ~p~n"
              "    view of node2 ~p~n"
              "    view of node3 ~p~n",
              [MasterMembers, Node1Members, Node2Members, Node3Members]),
    true = list_eqv(MasterMembers, [master@localhost, node3@localhost]),
    true = list_eqv(Node3Members, [master@localhost, node3@localhost]),
    true = list_eqv(Node1Members, [node1@localhost]),
    true = list_eqv(Node2Members, [node2@localhost]),
    ok = rpc:call(node3@localhost, partisan_peer_service, leave, [N3]),
    ok.

