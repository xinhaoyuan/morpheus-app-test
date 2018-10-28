-module(ra_test).

-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 3600, ?_test( test_entry() )}.

-define(config(Name, Config), proplists:get_value(Name, Config)).
-define(S, morpheus_sandbox).
-define(GH, morpheus_guest_helper).
-define(G, morpheus_guest).
-define(REPEAT, 100).

test_entry() ->
    Config = [ {server_id,  {tserver1, node()}}
             , {uid, <<"node1_uid">>}
             , {server_id2, {tserver2, node()}}
             , {uid2, <<"node2_uid">>}
             , {server_id3, {tserver3, node()}}
             , {uid3, <<"node3_uid">>}
             , {server_id4, {tserver4, node()}}
             , {uid4, <<"node4_uid">>}
             , {server_id5, {tserver5, node()}}
             , {uid5, <<"node5_uid">>}
             , {cluster_name, <<"cluster">>}
             , {priv_dir, "/tmp/ra"}
             ],

    {Ctl, MRef} =
        ?S:start(
           ?MODULE, test_sandbox_entry, [Config],
           [ monitor
           , {fd_opts,
              [{scheduler, {basicpos, []}}]}
           , {heartbeat, once}
           , {clock_offset, 1538099922306}
           , {clock_limit, ?REPEAT * 30000}
           , stop_on_deadlock
           %% , trace_send, trace_receive
           %% , verbose_handle, verbose_ctl
           %% , {trace_from_start, true}
           ]),
    ?assertEqual(success, receive {'DOWN', MRef, _, _, Reason} -> Reason end),
    ok.

test_sandbox_entry(Config) ->
    ?GH:bootstrap(),

    ok = application:load(ra),
    application:ensure_all_started(lg),

    ets:new(test, [public, named_table]),
    ets:insert(test, {counter, 0}),

    PrivDir = ?config(priv_dir, Config),

    ?GH:sync_task(
       [repeat, ?REPEAT,
        fun () ->
                lists:foreach(
                  fun (dummy_head) ->
                          ok;
                      (Case) ->
                          case PrivDir of
                              undefined ->
                                  io:format(user, "Skip cleanup~n");
                              [] ->
                                  io:format(user, "Skip cleanup due to empty priv dir~n");
                              _ ->
                                  Cmd = lists:flatten(
                                          io_lib:format(
                                            "rm -rf ~s/*",
                                            [PrivDir])),
                                  io:format(user, "Clean up with ~s~n", [Cmd]),
                                  os:cmd(Cmd)
                          end,

                          ra:start_in(PrivDir),

                          TC = ets:update_counter(test, counter, 1),
                          io:format(user, "Test ~w~n", [TC]),
                          Case(Config),
                          io:format(user, "Finished~n", []),

                          application:stop(ra),
                          timer:sleep(1000)
                  end, [ dummy_head
                       %% , fun call_from/1
                       , fun badkey_previous_cluster/1
                       ])
        end]),
    ets:delete(test),

    ?G:exit_with(success),
    ok.

%% fixed in 4c0c57b7c0ca793c7fdf98d7499f73593473decc
call_from(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    ok = start_cluster(ClusterName, Peers),
    test_tab = ets:new(test_tab, [public, named_table]),
    timer:sleep(10000),

    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],

    ?GH:sync_task([ par
                  , fun() ->
                            catch ra:stop_server(ServerId1),
                            catch ra:restart_server(ServerId1)
                    end
                  , fun () ->
                            catch ra:remove_member(ServerId1, ServerId2),
                            catch ra:add_member(ServerId1, ServerId2)
                    end
                  , fun () ->
                            R = (catch enqueue(ServerId3, msg1)),
                            ets:insert(test_tab, {msg1, R})
                    end
                  ]),
    timer:sleep(10000),
    case ets:lookup(test_tab, msg1) of
        [{_, ok}] ->
            Out =(catch dequeue(ServerId1)),
            case Out of
                msg1 ->
                    ok;
                _ ->
                    io:format(user, "Error, dequeue got ~p~n", [Out]),
                    error(dequeue_doesnt_match)
            end;
        _ ->
            ok
    end,
    ets:delete(test_tab),
    ok.

badkey_previous_cluster(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    ok = start_cluster(ClusterName, Peers),
    test_tab = ets:new(test_tab, [public, named_table]),
    timer:sleep(10000),

    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],

    ?GH:sync_task([ par
                  , fun() ->
                            catch ra:trigger_election(ServerId1)
                    end
                  , fun() ->
                            catch ra:trigger_election(ServerId2)
                    end
                  , fun() ->
                            catch ra:trigger_election(ServerId3)
                    end
                  , fun() ->
                            catch ra:trigger_election(ServerId4)
                    end
                  , fun () ->
                            catch ra:remove_member(ServerId1, ServerId2),
                            catch ra:add_member(ServerId1, ServerId2)
                    end
                  , fun () ->
                            catch ra:remove_member(ServerId1, ServerId3),
                            catch ra:add_member(ServerId1, ServerId3)
                    end
                  , fun () ->
                            catch ra:remove_member(ServerId5, ServerId4),
                            catch ra:add_member(ServerId5, ServerId4)
                    end
                  , fun () ->
                            R = (catch enqueue(ServerId3, msg1)),
                            ets:insert(test_tab, {msg1, R})
                    end
                  ]),

    timer:sleep(10000),
    case ets:lookup(test_tab, msg1) of
        [{_, ok}] ->
            Out = (catch dequeue(ServerId1)),
            case Out of
                msg1 ->
                    ok;
                _ ->
                    io:format(user, "Error, dequeue got ~p~n", [Out]),
                    error(dequeue_doesnt_match)
            end;
        [{_, R}] ->
            io:format(user, "Got enqueue result ~p~n", [R])
    end,
    ets:delete(test_tab),
    ok.

%% from ra_2_SUITE.erl
start_cluster(ClusterName, ServerIds, Config) ->
    {ok, Started, _} = ra:start_cluster(ClusterName,
                                        {module, ?MODULE, Config},
                                        ServerIds),
    ?assertEqual(lists:sort(ServerIds), lists:sort(Started)),
    ok.

start_cluster(ClusterName, ServerIds) ->
    start_cluster(ClusterName, ServerIds, #{}).

enqueue(Server, Msg) ->
    {ok, _, _} = ra:process_command(Server, {enq, Msg}),
    ok.

dequeue(Server) ->
    {ok, Res, _} = ra:process_command(Server, deq),
    Res.

%% ra_machine test impl
init(_) ->
    queue:new().

'apply'(_Meta, {enq, Msg}, Effects, State) ->
    {queue:in(Msg, State), Effects, ok};
'apply'(_Meta, deq, Effects, State0) ->
    case queue:out(State0) of
        {{value, Item}, State} ->
            {State, Effects, Item};
        {empty, _} ->
            {State0, Effects, empty}
    end;
'apply'(_Meta, {deq, Pid}, Effects, State0) ->
    case queue:out(State0) of
        {{value, Item}, State} ->
            {State, [{send_msg, Pid, Item, ra_event} | Effects], ok};
        {empty, _} ->
            {State0, Effects, ok}
    end.

state_enter(eol, State) ->
    [{send_msg, P, eol, ra_event} || {P, _} <- queue:to_list(State), is_pid(P)];
state_enter(_, _) ->
    [].

-endif.
