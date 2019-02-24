-module(test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

-define(config(Key, Data), proplists:get_value(Key, Data)).

try_getenv(Name, Handler, Default) ->
    case os:getenv(Name) of
        false ->
            Default;
        S -> Handler(S)
    end.

-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

test_entry() ->
    Config =
        [ {sched, try_getenv("SCHED", fun list_to_atom/1, basicpos)}
        , {acc_filename, try_getenv("ACC_FILENAME", fun (I) -> I end, "acc.dat")}
        , {testcase, try_getenv("TESTCASE", fun list_to_atom/1, dist_bootstrap)}
        , {dump, try_getenv("DUMP", fun (I) -> I =/= "" end, false)}
        ],
    TConfig =
        [ {acc_filename, ?config(acc_filename, Config)}
        , {find_races, true}
        , {extra_opts,
           maps:from_list(
             [ {verbose_race_info, true}
             , {verbose_racing_prediction_stat, true}
             ]
             ++ case os:getenv("LABELED_TRACE") of
                    false -> [];
                    "" -> [];
                    Pred -> [{unify, true}]
                end
            )}
        ]
        ++ case os:getenv("PRED") of
               false -> [];
               "" -> [];
               "path" ->
                   [ {path_coverage, true}
                   , {to_predict, true}
                   , {predict_by, path}
                   ];
               "ploc" ->
                   [ {line_coverage, true}
                   , {to_predict, true}
                   , {predict_by, ploc}
                   ]
           end
        ++ case os:getenv("LABELED_TRACE") of
               false -> [];
               "" -> [];
               Pred -> [{dump_traces, true}]
           end,
    {ok, Tracer} = morpheus_tracer:start_link(TConfig),
    MConfig =
        [ monitor
        , { fd_opts
          , [ { scheduler
              , {?config(sched, Config), []} }
            , verbose_final ] }
        , {node, node1@localhost}
        , {clock_limit, 600000}
        , {clock_offset, 1539105131938}
        , {aux_module, ?MODULE}
        , {aux_data, case os:getenv("SCOPED") of
                         false -> false;
                         [] -> false;
                         _ -> true
                     end}
        , stop_on_deadlock
          %% , trace_send, trace_receive
          %% , verbose_handle, verbose_ctl
          %% , {trace_from_start, true}
        , {tracer_pid, Tracer}
        ]
        ++ case os:getenv("ONLY_SEND") of
               false -> [];
               "" -> [];
               _ -> [{only_schedule_send, true}]
           end
        ++ case os:getenv("PRED") of
               false -> [];
               "" -> [];
               _ -> [{use_prediction, true}]
           end
        ++ case os:getenv("SCOPED") of
               false -> [];
               "" -> [];
               _ -> [{scoped_weight, 2}]
           end
        ,
    {Ctl, MRef} = morpheus_sandbox:start(
                    ?MODULE, t_sandbox_entry, [Config],
                    MConfig),
    receive {'DOWN', MRef, _, _, Reason} ->
            case ?config(dump, Config) of
                true -> morpheus_tracer:dump_trace(Tracer);
                false -> ok
            end,
            morpheus_tracer:stop(Tracer),
            success = Reason
    end,
    ok.

t_sandbox_entry(Config) ->
    FNAME = ?config(testcase, Config),
    apply(?MODULE, FNAME, [Config]),
    ?G:exit_with(success).

dist_bootstrap(_) ->
        Nodes = [Node1, Node2, Node3] =
        [node1@localhost, node2@localhost, node3@localhost],
    lists:foreach(fun (Node) ->
                          ?GH:bootstrap_remote(Node)
                  end, Nodes -- [Node1]),
    ?GH:bootstrap(Node1).
