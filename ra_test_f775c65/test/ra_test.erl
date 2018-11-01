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

string_to_term(String) ->
    {ok, Tokens, _EndLine} = erl_scan:string(String),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

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
              [{scheduler,
                { basicpos
                , case os:getenv("SEED_TERM") of
                      false -> [];
                      Term -> [{seed, string_to_term(Term)}]
                  end
                }}]}
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

run_test_fun(Config, F) ->
    PrivDir = ?config(priv_dir, Config),
    UseRaceWeighted = ?config(use_race_weighted, Config),
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
    F(Config),
    application:stop(ra),
    timer:sleep(1000),
    ok.

test_sandbox_entry(Config) ->
    ?GH:bootstrap(),

    ok = application:load(ra),
    application:ensure_all_started(lg),

    ets:new(test, [public, named_table]),
    ets:insert(test, {counter, 0}),

    ?GH:sync_task(
       [repeat, ?REPEAT,
        fun () ->
                lists:foreach(
                  fun (dummy_head) ->
                          ok;
                      (Case) ->

                          TC = ets:update_counter(test, counter, 1),
                          io:format(user, "Test ~w~n", [TC]),
                          run_test_fun(Config, Case),
                          io:format(user, "Finished~n", [])

                  end, [ dummy_head
                       %% %% Fixed
                       %% , fun call_from/1
                       %% SEED_TERM="{exrop,[215237787223549923|254186191907464752]}."
                       , fun badkey_previous_cluster/1
                       %% %% SEED_TERM="{exrop,[71340763580235082|255124149775943689]}."
                       %% , fun inconsistent_state/1
                       %% %% SEED_TERM="{seed, {exrop,[7091665519689774|77123832616578765]}}." 
                       %% , fun inconsistent_state_2/1
                       ])
        end]),
    %% run_test_fun(Config, fun exp_test/1),

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
                  , fun () ->
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
                  , fun () ->
                            catch ra:trigger_election(ServerId1)
                    end
                  , fun () ->
                            catch ra:trigger_election(ServerId2)
                    end
                  , fun () ->
                            catch ra:trigger_election(ServerId3)
                    end
                  , fun () ->
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

inconsistent_state(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    Msgs = [msg1, msg2, msg3, msg4, msg5],
    ok = start_cluster(ClusterName, Peers),
    test_tab = ets:new(test_tab, [public, named_table]),
    timer:sleep(10000),

    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],

    ?GH:sync_task([ par
                  , fun () ->
                            catch ra:trigger_election(ServerId1)
                    end
                  , fun () ->
                            catch ra:trigger_election(ServerId2)
                    end
                  , fun () ->
                            catch ra:trigger_election(ServerId3)
                    end
                  , fun () ->
                            catch ra:remove_member(ServerId4, ServerId5),
                            catch ra:add_member(ServerId4, ServerId5)
                    end
                  , fun () ->
                            lists:foreach(
                              fun (D) ->
                                      S = lists:nth(rand:uniform(length(Peers)), Peers),
                                      R = (catch enqueue(S, D)),
                                      ets:insert(test_tab, {D, R})
                              end, Msgs)
                    end
                  ]),

    timer:sleep(10000),

    Expected = (catch ra:local_query(ServerId1, fun (S) -> S end)),
    States = lists:foreach(
               fun (ServerId) ->
                       S = (catch ra:local_query(ServerId, fun (S) -> S end)),
                       case S =:= Expected of
                           true -> ok;
                           false ->
                               case {Expected, S} of
                                   {{'EXIT', _}, _} -> ok;
                                   {_, {'EXIT', _}} -> ok;
                                   {_, _} ->
                                       io:format(user,
                                                 "Inconsistent state~n"
                                                 "  ~p:~p~n"
                                                 "  ~p:~p~n",
                                                 [ServerId1, Expected, ServerId, S]),
                                       error(unexpected)
                               end
                       end
               end, Peers -- [ServerId1]),
    ets:delete(test_tab),
    ok.

is_sublist([], L) ->
    true;
is_sublist([H | T], []) ->
    false;
is_sublist([H | T], [H | TL]) ->
    is_sublist(T, TL);
is_sublist([H | T] = S, [HL | TL]) ->
    is_sublist(S, TL).

mutual_removal(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    Msgs = [msg1, msg2, msg3, msg4, msg5],
    ok = start_cluster(ClusterName, Peers),
    test_tab = ets:new(test_tab, [public, named_table]),
    timer:sleep(10000),

    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],

    ?GH:sync_task([ par
                  , fun () ->
                            catch ra:remove_member(ServerId2, ServerId3)
                    end
                  , fun () ->
                            catch ra:remove_member(ServerId3, ServerId2)
                    end
                  , fun () ->
                            lists:foreach(
                              fun (D) ->
                                      S = lists:nth(rand:uniform(length(Peers)), Peers),
                                      R = (catch enqueue(S, D)),
                                      ets:insert(test_tab, {D, R})
                              end, Msgs)
                    end
                  ]),

    catch ra:add_member(ServerId1, ServerId2),
    catch ra:add_member(ServerId1, ServerId3),

    timer:sleep(10000),

    Expected = (catch ra:local_query(ServerId1, fun (S) -> S end)),
    States = lists:foreach(
               fun (ServerId) ->
                       S = (catch ra:local_query(ServerId, fun (S) -> S end)),
                       case S =:= Expected of
                           true -> ok;
                           false ->
                               case {Expected, S} of
                                   {{'EXIT', _}, _} -> ok;
                                   {_, {'EXIT', _}} -> ok;
                                   {_, _} ->
                                       io:format(user,
                                                 "!!! ~p:~p~n"
                                                 "    ~p:~p~n",
                                                 [ServerId1, Expected, ServerId, S]),
                                       error(unexpected)
                               end
                       end
               end, Peers -- [ServerId1]),
    ets:delete(test_tab),
    ok.


inconsistent_state_2(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    Msgs = [msg1, msg2, msg3, msg4, msg5],
    ok = start_cluster(ClusterName, Peers),
    test_tab = ets:new(test_tab, [public, named_table]),
    timer:sleep(10000),

    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],

    ?GH:sync_task([ par
                  , fun () ->
                            R = (catch enqueue(ServerId3, msg1)),
                            R1 = (catch ra:remove_member(ServerId2, ServerId3)),
                            R2 = (catch ra:add_member(ServerId2, ServerId3)),
                            io:format(user, "remove_member ~p ~p => ~p~n", [ServerId2, ServerId3, R1]),
                            io:format(user, "   add_member ~p ~p => ~p~n", [ServerId2, ServerId3, R2]),
                            ets:insert(test_tab, {msg1, R}),
                            ok
                    end
                  , fun () ->
                            R = (catch enqueue(ServerId3, msg2)),
                            R1 = (catch ra:remove_member(ServerId4, ServerId3)),
                            R2 = (catch ra:add_member(ServerId4, ServerId3)),
                            io:format(user, "remove_member ~p ~p => ~p~n", [ServerId4, ServerId3, R1]),
                            io:format(user, "   add_member ~p ~p => ~p~n", [ServerId4, ServerId3, R2]),
                            ets:insert(test_tab, {msg2, R}),
                            ok
                    end
                  , fun () ->
                            catch ra:trigger_election(ServerId1)
                    end
                  , fun () ->
                            catch ra:trigger_election(ServerId5)
                    end
                  ]),
    %% catch ra:restart_server(ServerId3),

    timer:sleep(10000),

    Expected = (catch ra:local_query(ServerId1, fun (S) -> S end)),
    States = lists:foreach(
               fun (ServerId) ->
                       S = (catch ra:local_query(ServerId, fun (S) -> S end)),
                       case S =:= Expected of
                           true -> ok;
                           false ->
                               case {Expected, S} of
                                   {{'EXIT', _}, _} -> ok;
                                   {_, {'EXIT', _}} -> ok;
                                   {_, _} ->
                                       io:format(user,
                                                 "State result inconsistent~n"
                                                 "  ~p:~p~n"
                                                 "  ~p:~p~n",
                                                 [ServerId1, Expected, ServerId, S]),
                                       error(unexpected)
                               end
                       end
               end, Peers -- [ServerId1]),

    ExpectedResult =
        lists:foldl(
          fun (M, Acc) ->
                  case ets:lookup(test_tab, M) of
                      [{_, ok}] ->
                          [M | Acc];
                      _ ->
                          Acc
                  end
          end, [], [msg1, msg2]),
    DequeueResult =
        lists:foldl(
          fun (_, Acc) ->
                  [(catch dequeue(ServerId3)) | Acc]
          end, [], [msg1, msg2]),
    case [] =:= ExpectedResult -- DequeueResult of
        true -> ok;
        false ->
            io:format(user,
                      "Dequeue result inconsistent~n"
                      "  expect ~p~n"
                      "     got ~p~n", [ExpectedResult, DequeueResult]),
            error(unexpected)
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
