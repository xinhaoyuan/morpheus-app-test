-module(lager_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-compile({parse_transform, lager_transform}).

all_test_() ->
    {timeout, 3600, ?_test( test_entry() )}.

-define(config(K, C), proplists:get_value(K, C)).
-define(S, morpheus_sandbox).
-define(G, morpheus_guest).
-define(H, morpheus_guest_helper).

test_entry() ->
    Config = [ {repeat, 100}
             , {sched, basicpos}
             ],
    {Ctl, MRef} =
        ?S:start(
           ?MODULE, test_sandbox_entry, [Config],
           [ monitor
           , {fd_opts,
              [{scheduler,
                { ?config(sched, Config)
                , []
                }}]}
           , {heartbeat, once}
           , {clock_offset, 1538099922306}
           , {clock_limit, ?config(repeat, Config) * 30000 + 30000}
           , stop_on_deadlock
           %% , trace_send, trace_receive
           %% , verbose_handle, verbose_ctl
           %% , {trace_from_start, true}
           ]
          ),
    ?assertEqual(success, receive {'DOWN', MRef, _, _, Reason} -> Reason end),
    ok.

test_sandbox_entry(Config) ->
    ?H:bootstrap(),
    lager:start(),
    ?H:sync_task(
       [ repeat, ?config(repeat, Config),
         [ par,
           fun () ->
                   lager:info("Log 1", [])
           end,
           fun () ->
                   lager:critical("Log 2", [])
           end,
           fun () ->
                   lager:warning("Log 3", [])
           end
         ]]),
    timer:sleep(1000),
    ?G:exit_with(success),
    ok.
