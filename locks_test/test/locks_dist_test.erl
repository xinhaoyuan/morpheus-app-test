-module(locks_dist_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

%% ?MORPHEUS_CB_IS_SCOPED(true, erl_eval) ->
%%     true;
%% ?MORPHEUS_CB_IS_SCOPED(_, _) ->
%%     false.

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

-define(config(Key, Data), proplists:get_value(Key, Data)).

try_getenv(Name, Handler, Default) ->
    case os:getenv(Name) of
        false ->
            Default;
        S -> Handler(S)
    end.

test_entry() ->
    Config =
        [ {sched, try_getenv("SCHED", fun list_to_atom/1, basicpos)}
        , {repeat, try_getenv("REPEAT", fun list_to_integer/1, 100)}
        , {pred, try_getenv("PRED", fun list_to_atom/1, no)}
        , {pred_skip, try_getenv("PRED_SKIP", fun ("") -> false; (_) -> true end, false)}
        , {acc_filename, try_getenv("ACC_FILENAME", fun (I) -> I end, "acc.dat")}
        ],
    Pred = ?config(pred, Config),
    Tracer =
        case Pred of
            no -> undefined;
            _ ->
                {ok, _Tracer} =
                    morpheus_tracer:start_link(
                      [ {acc_filename, ?config(acc_filename, Config)}
                      , {find_races, true}
                      , {extra_opts,
                         maps:from_list(
                           [ {verbose_race_info, true}
                           , {verbose_racing_prediction_stat, true}
                           ]
                          )}
                      ]
                      ++ case Pred of
                             path ->
                                 [ {path_coverage, true}
                                 , {to_predict, true}
                                 , {predict_by, path}
                                 ];
                             ploc ->
                                 [ {line_coverage, true}
                                 , {to_predict, true}
                                 , {predict_by, ploc}
                                 ]
                         end
                     ),
                _Tracer
        end,
    MConfig =
        [ monitor
        , { fd_opts
          , [ { scheduler
              , {?config(sched, Config), []} }
            , verbose_final ] }
        , {node, node1@localhost}
        , {clock_limit, 600000}
        , {clock_offset, 1539105131938}
        ]
        ++ case Tracer of
               undefined -> [];
               _ -> [{tracer_pid, Tracer}]
           end
        ++ case Pred of
               no -> [];
               _ -> [{use_prediction, case ?config(pred_skip, Config) of true -> skip; false -> true end}]
           end
        ,
    {Ctl, MRef} = morpheus_sandbox:start(
                    ?MODULE, t_sandbox_entry, [Config],
                    MConfig),
    receive
        {'DOWN', MRef, _, _, Reason} ->
            case Tracer of
                undefined -> ok;
                _ -> morpheus_tracer:stop(Tracer)
            end,
            success = Reason
    end,
    ok.

-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

%% t_sandbox_entry(Config) ->
%%     Nodes = [Node1, Node2, Node3] =
%%         [node1@localhost, node2@localhost, node3@localhost],
%%     lists:foreach(fun (Node) ->
%%                           ?GH:bootstrap_remote(Node)
%%                   end, Nodes -- [Node1]),
%%     ?GH:bootstrap(Node1),
%%     ?G:set_flags([{tracing, true}]),

%%     {[ok, ok, ok], []} = rpc:multicall(Nodes, application, start, [locks]),

%%     tab = ets:new(tab, [named_table, public]),
%%     ets:insert(tab, {test_counter, 0}),

%%     {ok, ECBegin} = ?G:call_ctl({nodelay, {query, scheduler_push_counter}}),
%%     ?GH:sync_task(
%%        [ repeat, ?config(repeat, Config)
%%        , fun () ->
%%                  Cnt = ets:update_counter(tab, test_counter, 1),
%%                  io:format(user, "Test ~w~n", [Cnt]),
%%                  Workers =
%%                      lists:foldr(
%%                        fun (Id, Acc) ->
%%                                {Pid, MRef} =
%%                                    erlang:spawn_monitor(
%%                                      lists:nth(Id, Nodes),
%%                                      fun () ->
%%                                              {ok, Agt} = locks_agent:start(),
%%                                              locks:change_flag(Agt, abort_on_deadlock, true),
%%                                              locks:change_flag(Agt, await_nodes, true),
%%                                              LockOrder =
%%                                                  case Id of
%%                                                      1 -> [[2], [1]];
%%                                                      2 -> [[1], [2]]
%%                                                  end,
%%                                              lists:foreach(
%%                                                fun (Lock) ->
%%                                                        io:format("~p(~p) ~p lock ~p call ~p~n", [self(), Agt, Id, Lock, locks:lock(Agt, Lock, write, Nodes, all)])
%%                                                end, LockOrder)
%%                                      end),
%%                                [{Pid, MRef} | Acc]
%%                        end, [], lists:seq(1, 2)),

%%                  ok = rpc:call(Node3, application, stop, [locks]),
%%                  ok = rpc:call(Node3, application, start, [locks]),

%%                  lists:foreach(
%%                    fun ({_, MRef}) ->
%%                            receive {'DOWN', MRef, _, _, _} -> ok end
%%                    end, Workers),
%%                  ok
%%          end
%%        ]),
%%     {ok, ECEnd} = ?G:call_ctl({nodelay, {query, scheduler_push_counter}}),
%%     io:format(user, "Event counter = ~p~n", [ECEnd - ECBegin]),

%%     ?G:exit_with(success).

t_sandbox_entry(Config) ->
    Nodes = [Node1, Node2] =
        [node1@localhost, node2@localhost],
    lists:foreach(fun (Node) ->
                          ?GH:bootstrap_remote(Node)
                  end, Nodes -- [Node1]),
    ?GH:bootstrap(Node1),
    ?G:set_flags([{tracing, true}]),

    ok = application:start(locks),

    {ok, ECBegin} = ?G:call_ctl({nodelay, {query, scheduler_push_counter}}),
    {Pid, MRef} =
        spawn_monitor(
          fun () ->
                  {ok, Agt} = locks_agent:start(),
                  locks:change_flag(Agt, abort_on_deadlock, true),
                  locks:change_flag(Agt, await_nodes, true),
                  locks:lock(Agt, [1], write, Nodes, all)
          end),
    
    ok = rpc:call(Node2, application, start, [locks]),
    receive {'DOWN', MRef, _, _, _} -> ok end,

    {ok, ECEnd} = ?G:call_ctl({nodelay, {query, scheduler_push_counter}}),
    io:format(user, "Event counter = ~p~n", [ECEnd - ECBegin]),

    ?G:exit_with(success).
