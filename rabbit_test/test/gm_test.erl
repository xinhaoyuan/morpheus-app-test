%% This file is based on test/gm_SUITE.erl in github.com/rabbitmq/rabbitmq-server

-module(gm_test).

-behaviour(gm).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(config(K, D), proplists:get_value(K, D)).
-define(GH, morpheus_guest_helper).
-define(G, morpheus_guest).

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

test_entry() ->
    Config = [ %%
               {node_id,  {testcase_node1, node()}}
             , {node_id2, {testcase_node2, node()}}
             , {node_id3, {testcase_node3, node()}}
             , {cluster_id, <<"cluster">>}
             , {uid, <<"node_uid">>}
             , {priv_dir, "/tmp/rabbit"}
             , {repeat, 100}
             ],

    {Ctl, MRef} = morpheus_sandbox:start(
            ?MODULE, test_sandbox_entry, [Config],
            [ monitor
            , { fd_opts
                , [ { scheduler
                    , {basicpos,
                       [
                        {seed, {exrop,[41680694167095318|250536333419014672]}}
                        %% {seed, {exrop,[208137907084356975|280831899628933651]}}
                       ]} }
                  , verbose_final ] }
            , stop_on_deadlock
            , {heartbeat, once}
            , {clock_offset, 1543790191051}
            , {clock_limit, ?config(repeat, Config) * 10000 + 10000}
            ]),
    ?assertEqual(success, receive {'DOWN', MRef, _, _, Reason} -> Reason end),

    ok.

test_sandbox_entry(Config0) ->
    ?GH:bootstrap(),

    Config1 = init_per_suite(Config0),
    ets:new(test, [public, named_table]),
    ets:insert(test, {counter, 0}),

    ?GH:sync_task(
       [ repeat, ?config(repeat, Config1)
       , fun () ->
                 lists:foreach(fun (Fun) ->
                                       C = ets:update_counter(test, counter, 1),
                                       io:format(user, "Test ~p~n", [C]),
                                       apply(?MODULE, Fun, [Config1])
                               end,
                               [
                                my_test
                               ])
         end
       ]),

    end_per_suite(Config1),

    ?G:exit_with(success).

-define(RECEIVE_OR_THROW(Body, Bool, Error),
        receive Body ->
                true = Bool,
                passed
        after 1000 ->
                throw(Error)
        end).

all() ->
    [
      join_leave,
      broadcast,
      confirmed_broadcast,
      member_death,
      receive_in_order,
      unexpected_msg,
      down_in_members_change
    ].

init_per_suite(Config) ->
    ok = application:set_env(mnesia, dir, ?config(priv_dir, Config)),
    ok = application:start(mnesia),
    {ok, FHC} = file_handle_cache:start_link(),
    unlink(FHC),
    {ok, WPS} = worker_pool_sup:start_link(),
    unlink(WPS),
    [{file_handle_cache_pid, FHC},
     {worker_pool_sup_pid, WPS} | Config].

end_per_suite(Config) ->
    exit(?config(worker_pool_sup_pid, Config), shutdown),
    exit(?config(file_handle_cache_pid, Config), shutdown),
    ok = application:stop(mnesia),
    Config.

%% My original tests

my_test(Config) ->
    ok = gm:create_tables(),

    Pids = [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6] =
        lists:foldl(
          fun (_, Acc) ->
                  {ok, Pid} = gm:start_link(?MODULE, ?MODULE, self(),
                                            fun rabbit_misc:execute_mnesia_transaction/1),
                  [Pid | Acc]
          end, [], lists:seq(1, 6)),

    lists:foreach(
      fun (P) ->
              receive {joined, P, _} -> ok end
      end, Pids),

    ToRemove = [Pid2, Pid3, Pid4, Pid5],

    lists:foreach(fun (P) -> unlink(P) end, ToRemove),

    ?GH:sync_task(
       [par,
        fun () ->
                lists:foreach(fun (P) -> exit(P, kill) end, ToRemove)
        end,
        fun () ->
                ok = gm:broadcast(Pid6, msg1),
                ok = gm:broadcast(Pid6, msg2)
        end
       ]),

    timer:sleep(1000),

    lists:foreach(
      fun (P) ->
              receive {msg, P, _, M1} -> M1 = msg1 after 0 -> error({missing_msg, P, msg1}) end,
              receive {msg, P, _, M2} -> M2 = msg2 after 0 -> error({missing_msg, P, msg2}) end,
              receive {msg, P, _, M} -> error({unexpected_msg, P, M}) after 0 -> ok end,

              gm:leave(P)
      end, Pids -- ToRemove),

    (fun F() ->
            receive _ -> F() after 0 -> ok end
     end)(),

    ok.

%% ---------------------------------------------------------------------------
%% Functional tests
%% ---------------------------------------------------------------------------

join_leave(_Config) ->
    passed = with_two_members(fun (_Pid, _Pid2) -> passed end).

broadcast(_Config) ->
    passed = do_broadcast(fun gm:broadcast/2).

confirmed_broadcast(_Config) ->
    passed = do_broadcast(fun gm:confirmed_broadcast/2).

member_death(_Config) ->
    passed = with_two_members(
      fun (Pid, Pid2) ->
              {ok, Pid3} = gm:start_link(
                             ?MODULE, ?MODULE, self(),
                             fun rabbit_misc:execute_mnesia_transaction/1),
              M = monitor(process, Pid3),
              unlink(Pid3),
              %% exit(Pid3, kill),
              gm:leave(Pid3),
              passed = receive_termination(Pid3, normal, timeout_waiting_for_termination_3),
              receive
                  {'DOWN', M, _, _, _} -> ok
              end,

              passed = receive_joined(Pid3, [Pid, Pid2, Pid3],
                                      timeout_joining_gm_group_3),
              passed = receive_birth(Pid, Pid3, timeout_waiting_for_birth_3_1),
              passed = receive_birth(Pid2, Pid3, timeout_waiting_for_birth_3_2),


              %% Have to do some broadcasts to ensure that all members
              %% find out about the death.
              passed = (broadcast_fun(fun gm:confirmed_broadcast/2))(
                         Pid, Pid2),

              passed = receive_death(Pid, Pid3, timeout_waiting_for_death_3_1),
              passed = receive_death(Pid2, Pid3, timeout_waiting_for_death_3_2),

              passed
      end).

receive_in_order(_Config) ->
    passed = with_two_members(
      fun (Pid, Pid2) ->
              Numbers = lists:seq(1,1000),
              [begin ok = gm:broadcast(Pid, N), ok = gm:broadcast(Pid2, N) end
               || N <- Numbers],
              passed = receive_numbers(
                         Pid, Pid, {timeout_for_msgs, Pid, Pid}, Numbers),
              passed = receive_numbers(
                         Pid, Pid2, {timeout_for_msgs, Pid, Pid2}, Numbers),
              passed = receive_numbers(
                         Pid2, Pid, {timeout_for_msgs, Pid2, Pid}, Numbers),
              passed = receive_numbers(
                         Pid2, Pid2, {timeout_for_msgs, Pid2, Pid2}, Numbers),
              passed
      end).

unexpected_msg(_Config) ->
    passed = with_two_members(
	       fun(Pid, _) ->
		       Pid ! {make_ref(), old_gen_server_answer},
		       true = erlang:is_process_alive(Pid),
		       passed
	       end).

down_in_members_change(_Config) ->
    %% Setup
    ok = gm:create_tables(),
    {ok, Pid} = gm:start_link(?MODULE, ?MODULE, self(),
                              fun rabbit_misc:execute_mnesia_transaction/1),
    passed = receive_joined(Pid, [Pid], timeout_joining_gm_group_1),
    {ok, Pid2} = gm:start_link(?MODULE, ?MODULE, self(),
                               fun rabbit_misc:execute_mnesia_transaction/1),
    passed = receive_joined(Pid2, [Pid, Pid2], timeout_joining_gm_group_2),
    passed = receive_birth(Pid, Pid2, timeout_waiting_for_birth_2),

    %% Test. Simulate that the gm group is deleted (forget_group) while
    %% processing the 'DOWN' message from the neighbour
    process_flag(trap_exit, true),
    ok = meck:new(mnesia, [passthrough]),
    ok = meck:expect(mnesia, read, fun({gm_group, ?MODULE}) ->
					   [];
				      (Key) ->
					   meck:passthrough([Key])
				   end),
    gm:leave(Pid2),
    Passed = receive
		 {'EXIT', Pid, shutdown} ->
		     passed;
		 {'EXIT', Pid, _} ->
		     crashed
	     after 15000 ->
		     timeout
	     end,
    %% Cleanup
    meck:unload(mnesia),
    process_flag(trap_exit, false),
    passed = Passed.


do_broadcast(Fun) ->
    with_two_members(broadcast_fun(Fun)).

broadcast_fun(Fun) ->
    fun (Pid, Pid2) ->
            ok = Fun(Pid, magic_message),
            passed = receive_or_throw({msg, Pid, Pid, magic_message},
                                      timeout_waiting_for_msg),
            passed = receive_or_throw({msg, Pid2, Pid, magic_message},
                                      timeout_waiting_for_msg)
    end.

with_two_members(Fun) ->
    ok = gm:create_tables(),

    {ok, Pid} = gm:start_link(?MODULE, ?MODULE, self(),
                              fun rabbit_misc:execute_mnesia_transaction/1),
    passed = receive_joined(Pid, [Pid], timeout_joining_gm_group_1),

    {ok, Pid2} = gm:start_link(?MODULE, ?MODULE, self(),
                               fun rabbit_misc:execute_mnesia_transaction/1),
    passed = receive_joined(Pid2, [Pid, Pid2], timeout_joining_gm_group_2),
    passed = receive_birth(Pid, Pid2, timeout_waiting_for_birth_2),

    passed = Fun(Pid, Pid2),

    ok = gm:leave(Pid),
    passed = receive_death(Pid2, Pid, timeout_waiting_for_death_1),
    passed =
        receive_termination(Pid, normal, timeout_waiting_for_termination_1),

    ok = gm:leave(Pid2),
    passed =
        receive_termination(Pid2, normal, timeout_waiting_for_termination_2),

    receive X -> throw({unexpected_message, X})
    after 0 -> passed
    end.

receive_or_throw(Pattern, Error) ->
    ?RECEIVE_OR_THROW(Pattern, true, Error).

receive_birth(From, Born, Error) ->
    ?RECEIVE_OR_THROW({members_changed, From, Birth, Death},
                      ([Born] == Birth) andalso ([] == Death),
                      Error).

receive_death(From, Died, Error) ->
    ?RECEIVE_OR_THROW({members_changed, From, Birth, Death},
                      ([] == Birth) andalso ([Died] == Death),
                      Error).

receive_joined(From, Members, Error) ->
    ?RECEIVE_OR_THROW({joined, From, Members1},
                      lists:usort(Members) == lists:usort(Members1),
                      Error).

receive_termination(From, Reason, Error) ->
    ?RECEIVE_OR_THROW({termination, From, Reason1},
                      Reason == Reason1,
                      Error).

receive_numbers(_Pid, _Sender, _Error, []) ->
    passed;
receive_numbers(Pid, Sender, Error, [N | Numbers]) ->
    ?RECEIVE_OR_THROW({msg, Pid, Sender, M},
                      M == N,
                      Error),
    receive_numbers(Pid, Sender, Error, Numbers).

%% -------------------------------------------------------------------
%% gm behavior callbacks.
%% -------------------------------------------------------------------

joined(Pid, Members) ->
    Pid ! {joined, self(), Members},
    ok.

members_changed(Pid, Births, Deaths) ->
    Pid ! {members_changed, self(), Births, Deaths},
    ok.

handle_msg(Pid, From, Msg) ->
    Pid ! {msg, self(), From, Msg},
    ok.

handle_terminate(Pid, Reason) ->
    Pid ! {termination, self(), Reason},
    ok.
