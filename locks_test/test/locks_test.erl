-module(locks_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

is_scoped(locks_agent) ->
    true;
is_scoped(_) ->
    false.

test_entry() ->
    Sched =
        case os:getenv("SCHED") of
            false -> basicpos;
            _S -> list_to_atom(_S)
        end,
    MConfig =
        [ monitor
        , { fd_opts
          , [ { scheduler
              , {Sched, []} }
            , verbose_final ] }
        , {node, node1@localhost}
        , {clock_limit, 600000}
        , {clock_offset, 1539105131938}
        , {aux_module, ?MODULE}
        , stop_on_deadlock
          %% , trace_send, trace_receive
          %% , verbose_handle, verbose_ctl
          %% , {trace_from_start, true}
        ]
        ++ case os:getenv("ONLY_SEND") of
               false -> [];
               "" -> [];
               _ -> [{only_schedule_send, true}]
           end,
    {Ctl, MRef} = morpheus_sandbox:start(
                    ?MODULE, t_sandbox_entry, [],
                    MConfig),
    success = receive {'DOWN', MRef, _, _, Reason} -> Reason end,
    ok.

-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

t_sandbox_entry() ->
    ?GH:bootstrap(),

    ?G:set_flags([{tracing, true}]),
    ok = application:start(locks),
    ?GH:sync_task(
       [ repeat, 1
       , fun t1/0
       ]),
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
