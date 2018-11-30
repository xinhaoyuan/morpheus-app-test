-module(ra_test).

-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 10, ?_test( test_entry() )}.

-define(config(Name, Config), proplists:get_value(Name, Config)).
-define(vcall(E), (io:format(user, "~w: ~s => ~p~n", [?LINE, ??E, E]))).

string_to_term(String) ->
    {ok, Tokens, _EndLine} = erl_scan:string(String),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

test_entry() ->
    Config = [ {server_id, {tserver1, node()}}
             , {uid, <<"node1_uid">>}
             , {server_id2, {tserver2, node()}}
             , {uid2, <<"node2_uid">>}
             , {server_id3, {tserver3, node()}}
             , {uid3, <<"node3_uid">>}
             , {server_id4, {tserver4, node()}}
             , {uid4, <<"node4_uid">>}
             , {server_id5, {tserver5, node()}}
             , {uid5, <<"node5_uid">>}
             , {cluster_name, <<"cluster">>}
             , {priv_dir, "/tmp/ra"}
             ],

    ok = application:load(ra),
    application:ensure_all_started(lg),
    run_test_fun(Config, inconsistent_state_2),
    ok.

run_test_fun(Config, FName) ->
    PrivDir = ?config(priv_dir, Config),
    case PrivDir of
        undefined ->
            io:format(user, "Skip cleanup~n");
        [] ->
            io:format(user, "Skip cleanup due to empty priv dir~n");
        _ ->
            Cmd = lists:flatten(
                    io_lib:format(
                      "rm -rf ~s/*",
                      [PrivDir])),
            io:format(user, "Clean up with ~s~n", [Cmd]),
            os:cmd(Cmd)
    end,

    ra:start_in(PrivDir),
    apply(?MODULE, FName, [Config]),
    application:stop(ra),
    ok.

inconsistent_state_2(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    ok = start_cluster(ClusterName, Peers),

    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],

    timer:sleep(1000),
    ra:trigger_election(ServerId5),
    timer:sleep(1000),

    %% Assume ServerId5 is the leader
    {ok, undefined, ServerId5} = ra:consistent_query(ServerId5, fun (_) -> undefined end),

    ?vcall(ra:stop_server(ServerId1)),
    ?vcall(enqueue(ServerId5, msg1)),
    timer:sleep(250),
    ?vcall(ra:restart_server(ServerId1)),
    erlang:suspend_process(whereis(tserver1)),
    %% The append_entries_request to server 1 will be rejected (likely) due to the missing first entry, which will trigger the error
    ?vcall(enqueue(ServerId5, msg1)),
    ?vcall(ra:remove_member(ServerId5, ServerId1)),
    timer:sleep(250),
    %% Rejection will happen now
    erlang:resume_process(whereis(tserver1)),

    timer:sleep(5000),
    ok.

%% from ra_2_SUITE.erl
start_cluster(ClusterName, ServerIds, Config) ->
    {ok, Started, _} = ra:start_cluster(ClusterName,
                                        {module, ?MODULE, Config},
                                        ServerIds),
    ?assertEqual(lists:sort(ServerIds), lists:sort(Started)),
    ok.

start_cluster(ClusterName, ServerIds) ->
    start_cluster(ClusterName, ServerIds, #{}).

enqueue(Server, Msg) ->
    {ok, _, _} = ra:process_command(Server, {enq, Msg}),
    ok.

dequeue(Server) ->
    {ok, Res, _} = ra:process_command(Server, deq),
    Res.

%% ra_machine test impl
init(_) ->
    queue:new().

'apply'(_Meta, {enq, Msg}, Effects, State) ->
    {queue:in(Msg, State), Effects, ok};
'apply'(_Meta, deq, Effects, State0) ->
    case queue:out(State0) of
        {{value, Item}, State} ->
            {State, Effects, Item};
        {empty, _} ->
            {State0, Effects, empty}
    end;
'apply'(_Meta, {deq, Pid}, Effects, State0) ->
    case queue:out(State0) of
        {{value, Item}, State} ->
            {State, [{send_msg, Pid, Item, ra_event} | Effects], ok};
        {empty, _} ->
            {State0, Effects, ok}
    end.

state_enter(eol, State) ->
    [{send_msg, P, eol, ra_event} || {P, _} <- queue:to_list(State), is_pid(P)];
state_enter(_, _) ->
    [].

-endif.
