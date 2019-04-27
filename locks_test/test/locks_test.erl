-module(locks_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

%% %% Override gen_server loop entry to extract states
%% ?MORPHEUS_CB_TO_OVERRIDE(_, gen_server, loop, 7) ->
%%     {true, callback};
%% ?MORPHEUS_CB_TO_OVERRIDE(_, locks_agent, loop, 1) ->
%%     {true, callback};
%% ?MORPHEUS_CB_TO_OVERRIDE(_, _, _, _) ->
%%     false.

%% ?MORPHEUS_CB_HANDLE_OVERRIDE(_, gen_server, NewModule, loop, NewEntry, Args, _Ann) ->
%%     %% This is a bit of hacky to extract info from the arguments
%%     %% The reporting interface in callback is not stable yet ...
%%     [_, Name, State, Mod | _] = Args,
%%     case Mod of
%%         locks_server ->
%%             ets:delete(test_state, self()),
%%             ets:insert(test_state, {self(), State}),
%%             ToReport = lists:sort(ets:match(test_state, '$1')),
%%             %% UGLY! ...
%%             morpheus_sandbox:call_ctl(morpheus_sandbox:get_ctl(), undefined, {nodelay, {guest_report_state, ToReport}}),
%%             %% io:format(user, "report state ~p~n", [ToReport]),
%%             ok;
%%         %% locks_agent seems to have its own loop ...
%%         %% locks_agent -> ok;
%%         _ -> ok
%%     end,
%%     %% forward to the original code
%%     apply(NewModule, NewEntry, Args);
%% ?MORPHEUS_CB_HANDLE_OVERRIDE(_, locks_agent, NewModule, loop, NewEntry, Args, _Ann) ->
%%     [State] = Args,
%%     ets:delete(test_state, self()),
%%     ets:insert(test_state, {self(), State}),
%%     ToReport = lists:sort(ets:match(test_state, '$1')),
%%     %% UGLY! ...
%%     morpheus_sandbox:call_ctl(morpheus_sandbox:get_ctl(), undefined, {nodelay, {guest_report_state, ToReport}}),
%%     %% io:format(user, "report state ~p~n", [ToReport]),
%%     %% forward to the original code
%%     apply(NewModule, OrigLoop, Args).

%% ?MORPHEUS_CB_IS_SCOPED(true, locks_agent) ->
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
        , {clock_limit, 5000 + 5000 * ?config(repeat, Config)}
        , {clock_offset, 1539105131938}
        %% , {aux_module, ?MODULE}
        %% , {aux_data, case os:getenv("SCOPED") of
        %%                  false -> false;
        %%                  [] -> false;
        %%                  _ -> true
        %%              end}
        %% , stop_on_deadlock
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

t_sandbox_entry(Config) ->
    ?GH:bootstrap(),

    ?G:set_flags([{tracing, true}]),
    ok = application:start(locks),

    {ok, ECBegin} = ?G:call_ctl({nodelay, {query, scheduler_push_counter}}),
    ?GH:sync_task(
       [ repeat, ?config(repeat, Config)
       , fun t1/0
       ]),
    {ok, ECEnd} = ?G:call_ctl({nodelay, {query, scheduler_push_counter}}),
    io:format(user, "Event counter = ~p~n", [ECEnd - ECBegin]),
    ?G:exit_with(success).

t1() ->
    Me = self(),
    Tab = ets:new(test_tab, [public]),
    Size = 3,
    Locks = lists:map(fun (E) -> [E] end, lists:seq(1,Size)),
    lists:foreach(fun (Id) -> spawn(fun () -> t1_c(Me, Tab, Id, Locks) end) end, lists:seq(1,Size)),
    lists:foreach(fun (Id) -> receive {Id, ok} -> io:format("~p~n", [{Id, finished}]) end end, lists:seq(1,Size)),
    ets:delete(Tab).

t1_c(CtrlProc, Tab, Id, Locks) ->
    {ok, Agt} = locks_agent:start(),
    LockOrder = case Id of
                    1 -> [[1], [2], [3]];
                    2 -> [[2], [3], [1]];
                    3 -> [[3], [1], [2]]
                end,
    io:format("~p~n ~p~n", [self(), LockOrder]),
    lists:foreach(
      fun (Lock) ->
              io:format("~p(~p) ~p lock ~p call ~p~n", [self(), Agt, Id, Lock, locks:lock(Agt, Lock, write)])
      end, LockOrder),
    locks:end_transaction(Agt),
    CtrlProc ! {Id, ok}.
