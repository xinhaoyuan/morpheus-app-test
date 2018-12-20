-module(gproc_test).

-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 3600, ?_test( test_entry() )}.


string_to_term(String) ->
    {ok, Tokens, _EndLine} = erl_scan:string(String),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

-define(config(Key, Data), proplists:get_value(Key, Data)).

try_getenv(Name, Handler, Default) ->
    case os:getenv(Name) of
        false ->
            Default;
        S -> Handler(S)
    end.

test_entry() ->
    Config0 =
        [ {repeat, try_getenv("REPEAT", fun list_to_integer/1, 100)}
        , {testcase, try_getenv("TESTCASE", fun list_to_atom/1, t_simple_ensure_other)}
        , {nodes, [node1@localhost, node2@localhost, node3@localhost]}
        , {sched, try_getenv("SCHED", fun list_to_atom/1, basicpos)}
        ],
    Config =
        Config0
        ++ [{reset_gproc,
             case ?config(testcase, Config0) of
                 t_simple_ensure_other -> true;
                 t_simple_reg -> true;
                 t_master_dies -> false;
                 _ -> false
             end}],
    io:format(user, "Test config = ~p~n", [Config]),
    case os:getenv("TEST_WD") of
        false -> ok;
        Dir -> c:cd(Dir)
    end,
    lists:foreach(
      fun (N) ->
              Cmd = lists:flatten(
                      io_lib:format("rm gproc_dist_~s", [N])
                     ),
              os:cmd(Cmd)
      end, ?config(nodes, Config)),
    test_state_tab = ets:new(test_state_tab, [public, named_table]),
    {Ctl, MRef} = morpheus_sandbox:start(
                    ?MODULE, test_sandbox_entry, [Config],
                    [ monitor
                    , {heartbeat, once}
                    , { fd_opts
                      , [ verbose_final
                        , { scheduler
                          , {?config(sched, Config),
                             []
                             ++ try_getenv("SEED_TERM", fun (S) -> [{seed, string_to_term(S)}] end, [])
                             ++ try_getenv("LOW_WATERMARK", fun (S) -> [{low_watermark, list_to_float(S)}] end, [])
                             ++ try_getenv("VARIANT", fun (S) -> [{variant, list_to_atom(S)}] end, [])
                            }
                          }
                        ]}
                    , stop_on_deadlock
                    , {node, master@localhost}
                    , {clock_limit, 10000 + ?config(repeat, Config) * 10000}
                    %% , trace_receive, trace_send
                    %% , verbose_handle
                    %% , verbose_ctl
                    %% , {trace_from_start, true}
                    ]
                    ++ case os:getenv("ONLY_SEND") of
                           false -> [];
                           "" -> [];
                           _ -> [{only_schedule_send, true}]
                       end
                    ++ case os:getenv("STATE_COVERAGE") of
                           false -> [];
                           "" -> [];
                           _ -> [{tracer_opts, [{acc_filename, "acc.dat"}, {acc_fork_period, 100}, {state_coverage, true}]}]
                       end
                   ),
    success = receive {'DOWN', MRef, _, _, Reason} -> Reason end,
    ok.

-define(GH, morpheus_guest_helper).
-define(G, morpheus_guest).

test_sandbox_entry(Config) ->
    Ns = proplists:get_value(nodes, Config),
    lists:foreach(fun (N) ->
                          ?GH:bootstrap_remote(N)
                  end, Ns),
    ?GH:bootstrap(master@localhost),

    {[ok,ok,ok],[]} = rpc:multicall(Ns, application, set_env,
                                    [gproc, gproc_dist, Ns]),

    case ?config(reset_gproc, Config) of
        true ->
            ok;
        false ->
            %% sequential start for minimal error surface
            lists:foreach(
              fun (N) ->
                      ok = rpc:call(N, application, ensure_started, [gproc]),
                      timer:sleep(250)
              end, Ns)
    end,

    {ok, ECBegin} = ?G:call_ctl({nodelay, {query, scheduler_push_counter}}),

    Tab = ets:new(test_tab, [public]),

    UseRaceWeighted = proplists:get_value(use_race_weighted, Config),
    ets:insert(Tab, {rep_counter, 0}),

    ?GH:sync_task(
       [ repeat, ?config(repeat, Config)
       , fun () ->
                 RC = ets:update_counter(Tab, rep_counter, 1),
                 io:format(user, "Test ~w starts~n", [RC]),

                 case ?config(reset_gproc, Config) of
                     true ->
                         {[ok,ok,ok],[]} = rpc:multicall(Ns, application, ensure_started, [gproc]);
                     false ->
                         lists:foreach(
                           fun (N) ->
                                   ok = rpc:call(N, application, ensure_started, [gproc]),
                                   timer:sleep(250)
                           end, Ns)
                 end,

                 Testcase = ?config(testcase, Config),
                 apply(?MODULE, Testcase, [Ns]),

                 timer:sleep(1000),
                 io:format(user, "Test ~w ends~n", [RC]),

                 case ?config(reset_gproc, Config) of
                     true ->
                         lists:foreach(
                           fun (N) ->
                                   ok = rpc:call(N, application, stop, [gproc])
                           end, Ns);
                     false -> ok
                 end
         end
       ]),

    ets:delete(Tab),

    {ok, ECEnd} = ?G:call_ctl({nodelay, {query, scheduler_push_counter}}),
    io:format(user, "Event counter = ~p~n", [ECEnd - ECBegin]),

    ?G:exit_with(success),
    ok.

-define(T_NAME, {n, g, {?MODULE, ?LINE, os:timestamp()}}).
-define(T_KVL, [{foo, "foo"}, {bar, "bar"}]).
-define(T_COUNTER, {c, g, {?MODULE, ?LINE}}).
-define(T_RESOURCE, {r, g, {?MODULE, ?LINE}}).
-define(T_PROP, {p, g, ?MODULE}).

t_simple_ensure_other([A, B|_] = Ns) ->
    Name = ?T_NAME,
    P1 = t_spawn(A),
    P2 = t_spawn(B),
    ?assertMatch(true, t_call(P1, {apply, gproc, reg_other, [Name, P2]})),
    ?assertMatch(ok, t_lookup_everywhere(Name, Ns, P2)),
    ?assertMatch(
       updated, t_call(
                  P1, {apply, gproc, ensure_reg_other, [Name, P2, new_val]})),
    ?assertMatch(ok, t_read_everywhere(Name, P2, Ns, new_val)),
    ?assertMatch(true, t_call(P1, {apply, gproc, unreg_other, [Name, P2]})),
    ?assertMatch(ok, t_lookup_everywhere(Name, Ns, undefined)),
    ?assertMatch(ok, t_call(P1, die)),
    ?assertMatch(ok, t_call(P2, die)).

try_sync(N, Ns) ->
    io:format(user, "call ~p:gproc_dist:sync~n", [N]),
    case rpc:call(N, gproc_dist, sync, []) of
        {badrpc, _} = Err ->
            ?debugFmt(
               "Error in gproc_dist:sync() (~p):~n"
               "  ~p~n"
               "Status = ~p~n",
               [Err, N,
                {Ns, rpc:multicall([N|Ns], sys, get_status, [gproc_dist])}]),
            Err;
        true ->
            true
    end.

t_sleep() ->
    timer:sleep(500).

t_lookup_everywhere(Key, Nodes, Exp) ->
    true = rpc:call(hd(Nodes), gproc_dist, sync, []),
    t_lookup_everywhere(Key, Nodes, Exp, 10).

t_lookup_everywhere(Key, _, Exp, 0) ->
    {lookup_failed, Key, Exp};
t_lookup_everywhere(Key, Nodes, Exp, I) ->
    Expected = [{N, Exp} || N <- Nodes],
    Found = [{N,rpc:call(N, gproc, where, [Key])} || N <- Nodes],
    if Expected =/= Found ->
	    ?debugFmt("lookup ~p failed~n"
		      "(Expected: ~p;~n"
		      " Found   : ~p)~n"
                      "status = ~p:~p, retrying...~n",
		      [Key, Expected, Found, rpc:multicall(Nodes, sys, get_status, [gproc_dist]), rpc:multicall(Nodes, sys, get_status, [gproc])]),
	    t_sleep(),
	    t_lookup_everywhere(Key, Nodes, Exp, I-1);
       true ->
	    ok
    end.

t_read_everywhere(Key, Pid, Nodes, Exp) ->
    true = rpc:call(hd(Nodes), gproc_dist, sync, []),
    t_read_everywhere(Key, Pid, Nodes, Exp, 3).

t_read_everywhere(Key, _, _, Exp, 0) ->
    {read_failed, Key, Exp};
t_read_everywhere(Key, Pid, Nodes, Exp, I) ->
    Expected = [{N, Exp} || N <- Nodes],
    Found = [{N, read_result(rpc:call(N, gproc, get_value, [Key, Pid]))}
	     || N <- Nodes],
    if Expected =/= Found ->
	    ?debugFmt("read ~p failed~n"
		      "(Expected: ~p;~n"
		      " Found   : ~p), retrying...~n",
		      [{Key, Pid}, Expected, Found]),
	    t_sleep(),
	    t_read_everywhere(Key, Pid, Nodes, Exp, I-1);
       true ->
	    ok
    end.

t_master_dies([A,B,C] = Ns) ->
    Na = ?T_NAME,
    Nb = ?T_NAME,
    Nc = ?T_NAME,
    Pa = t_spawn_reg(A, Na),
    Pb = t_spawn_reg(B, Nb),
    Pc = t_spawn_reg(C, Nc),
    L = rpc:call(A, gproc_dist, get_leader, []),
    ?assertMatch(ok, t_lookup_everywhere(Na, Ns, Pa)),
    ?assertMatch(ok, t_lookup_everywhere(Nb, Ns, Pb)),
    ?assertMatch(ok, t_lookup_everywhere(Nc, Ns, Pc)),
    {Nl, Pl} = case L of
                   A -> {Na, Pa};
                   B -> {Nb, Pb};
                   C -> {Nc, Pc}
               end,
    ?assertMatch(true, rpc:call(A, gproc_dist, sync, [])),
    ?assertMatch(ok, rpc:call(L, application, stop, [gproc])),
    Names = [{Na,Pa}, {Nb,Pb}, {Nc,Pc}] -- [{Nl, Pl}],
    RestNs = Ns -- [L],
    %% ?assertMatch(true, rpc:call(hd(RestNs), gproc_dist, sync, [])),
    ?assertMatch(true, try_sync(hd(RestNs), RestNs)),
    ?assertMatch(ok, t_lookup_everywhere(Nl, RestNs, undefined)),
    [?assertMatch(ok, t_lookup_everywhere(Nx, RestNs, Px))
     || {Nx, Px} <- Names],
    ok.

t_simple_reg([H|_] = Ns) ->
    Name = ?T_NAME,
    P = t_spawn_reg(H, Name),
    ?assertMatch(ok, t_lookup_everywhere(Name, Ns, P)),
    ?assertMatch(true, t_call(P, {apply, gproc, unreg, [Name]})),
    ?assertMatch(ok, t_lookup_everywhere(Name, Ns, undefined)),
    ?assertMatch(ok, t_call(P, die)).

read_result({badrpc, {'EXIT', {badarg, _}}}) -> badarg;
read_result(R) -> R.

t_spawn(Node) -> gproc_test_lib:t_spawn(Node).
t_spawn(Node, Selective) -> gproc_test_lib:t_spawn(Node, Selective).
t_spawn_mreg(Node, KVL) -> gproc_test_lib:t_spawn_mreg(Node, KVL).
t_spawn_reg(Node, N) -> gproc_test_lib:t_spawn_reg(Node, N).
t_spawn_reg(Node, N, V) -> gproc_test_lib:t_spawn_reg(Node, N, V).
t_spawn_reg(Node, N, V, As) -> gproc_test_lib:t_spawn_reg(Node, N, V, As).
t_spawn_reg_shared(Node, N, V) -> gproc_test_lib:t_spawn_reg_shared(Node, N, V).
got_msg(P) -> gproc_test_lib:got_msg(P).
got_msg(P, Tag) -> gproc_test_lib:got_msg(P, Tag).
no_msg(P, Timeout) -> gproc_test_lib:no_msg(P, Timeout).

t_call(P, Req) ->
    gproc_test_lib:t_call(P, Req).

-endif.
