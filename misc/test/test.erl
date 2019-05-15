-module(test).

-include_lib("firedrill/include/firedrill.hrl").
-include_lib("morpheus/include/morpheus.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

all_test_() ->
    {timeout, 86400, ?_test( test_entry() )}.

-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).
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
        , {acc_filename, try_getenv("ACC_FILENAME", fun (I) -> I end, "acc.dat")}
        , {dump, try_getenv("DUMP", fun (I) -> I =/= "" end, false)}
        , {testcase, try_getenv("TESTCASE", fun list_to_atom/1, undefined)}
        , {pred, try_getenv("PRED", fun list_to_atom/1, no)}
        , {profile_po, try_getenv("PROFILE_PO", fun ("") -> false; (_) -> true end, false)}
        ],
    Pred = ?config(pred, Config),
    Tracer =
        case Pred =/= no orelse ?config(profile_po, Config) of
            false -> undefined;
            true ->
                {ok, _Tracer} = morpheus_tracer:start_link(
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
                                  ++ case Pred of
                                         no -> [];
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
                                  ++ case os:getenv("LABELED_TRACE") of
                                         false -> [];
                                         "" -> [];
                                         Pred -> [{dump_traces, true}]
                                     end
                                  ++ case ?config(profile_po, Config) of
                                         true ->
                                             [{po_coverage, true}];
                                         false ->
                                             []
                                     end
                                 ),
                _Tracer
        end,
                                  
    MConfig =
        [ monitor
        , { fd_opts
           , [ { scheduler
              , {?config(sched, Config),
                 [
                 ]} }
            , verbose_final ] }
        , {node, node1@localhost}
        , {clock_limit, 60000}
        , {clock_offset, 1539105131938}
        , {aux_module, ?MODULE}
        , stop_on_deadlock
        %% , trace_send, trace_receive, trace_from_start
        , {heartbeat, none}
        ]
        ++ case Tracer of
               undefined -> [];
               _ -> [{tracer_pid, Tracer}]
           end
        ++ case Pred of
               no -> [];
               _ -> [{use_prediction, true}]
           end
        ,
    {Ctl, MRef} = morpheus_sandbox:start(
                    ?MODULE, t_sandbox_entry, [Config],
                    MConfig),
    receive {'DOWN', MRef, _, _, Reason} ->
            case Tracer of
                undefined -> ok;
                _ ->
                    morpheus_tracer:stop(Tracer)
            end,
            success = Reason
    end,
    ok.

t_sandbox_entry(Config) ->
    TC = ?config(testcase, Config),
    case TC of
        undefined ->
            io:format(user, "No test case selected~n", []); 
        _-> 
            apply(?MODULE, TC, [Config])
    end,
    ?G:exit_with(success).

simple_1(Config) ->
    Self = self(),
    spawn(fun () -> Self ! a end),
    spawn(fun () -> Self ! a end),
    receive a -> ok end,
    receive a -> ok end,
    ok.
