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

test_entry() ->
    Config = [{use_race_weighted, os:getenv("USE_RACE_WEIGHTED") =/= false}],
    case os:getenv("TEST_WD") of
        false -> ok;
        Dir -> c:cd(Dir)
    end,
    {Ctl, MRef} = morpheus_sandbox:start(
                    ?MODULE, test_sandbox_entry, [Config],
                    [ monitor
                    , {heartbeat, once}
                    , { fd_opts
                      , [ verbose_final
                        , { scheduler
                          , {basicpos,
                             case os:getenv("SEED_TERM") of
                                 false -> [];
                                 Term -> [{seed, string_to_term(Term)}]
                             end}
                          }
                        ]}
                    , stop_on_deadlock
                    , {node, master@localhost}
                    , {clock_limit, 100000}
                    %% , trace_receive, trace_send
                    %% , verbose_handle, verbose_ctl
                    %% , {trace_from_start, true}
                    ]),
    success = receive {'DOWN', MRef, _, _, Reason} -> Reason end,
    ok.

-define(GH, morpheus_guest_helper).
-define(G, morpheus_guest).

test_sandbox_entry(Config) ->
    ?GH:bootstrap_remote(node1@localhost),
    ?GH:bootstrap_remote(node2@localhost),
    ?GH:bootstrap_remote(node3@localhost),
    ?GH:bootstrap(master@localhost),

    Ns = [node1@localhost, node2@localhost, node3@localhost],

    {[ok,ok,ok],[]} = rpc:multicall(Ns, application, set_env,
                                    [gproc, gproc_dist, Ns]),
    {[ok,ok,ok],[]} = rpc:multicall(Ns, application, start, [gproc]),

    Tab = ets:new(test_tab, [public]),

    UseRaceWeighted = proplists:get_value(use_race_weighted, Config),
    ets:insert(Tab, {rep_counter, 0}),

    ?GH:sync_task(
       [ repeat, 100
       , fun () ->
                 RC = ets:update_counter(Tab, rep_counter, 1),
                 io:format(user, "Test ~w~n", [RC]),
                 ?G:set_flags([{tracing, true}]),
                 case UseRaceWeighted andalso RC > 50 of
                     true ->
                         io:format(user, "set race_weighted true~n", []),
                         ?G:set_flags([{race_weighted, true}]);
                     false ->
                         io:format(user, "set race_weighted false~n", []),
                         ?G:set_flags([{race_weighted, false}])
                 end,
                 t_simple_ensure_other(Ns)
         end
       ]),

    ets:delete(Tab),

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
