-module(locks_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

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
        ],
    MConfig =
        [ monitor
        , { fd_opts
          , [ { scheduler
              , {?config(sched, Config), []} }
            , verbose_final ] }
        , {node, node1@localhost}
        , {clock_limit, 5000 + 5000 * ?config(repeat, Config)}
        , {clock_offset, 1539105131938}
        , stop_on_deadlock
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

t_sandbox_entry(Config) ->
    ?GH:bootstrap(),

    ?G:set_flags([{tracing, true}]),
    ok = application:start(locks),
    ?GH:sync_task(
       [ repeat, ?config(repeat, Config)
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
