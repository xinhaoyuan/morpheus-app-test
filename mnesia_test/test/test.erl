-module(test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

all_test_() ->
    {timeout, 86400, ?_test( test_entry() )}.

-define(config(Key, Data), proplists:get_value(Key, Data)).

try_getenv(Name, Handler, Default) ->
    case os:getenv(Name) of
        false ->
            Default;
        S -> Handler(S)
    end.

-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

-record(test_record, { key, key2, value, value2 }).

test_entry() ->
    Config0 =
        [ {sched, try_getenv("SCHED", fun list_to_atom/1, basicpos)}
        , {acc_filename, try_getenv("ACC_FILENAME", fun (I) -> I end, "acc.dat")}
        , {testcase, try_getenv("TESTCASE", fun list_to_atom/1, del_and_restart)}
        , {dump, try_getenv("DUMP", fun (I) -> I =/= "" end, false)}
        ],
    Config = Config0
        ++ case ?config(testcase, Config0) of
               del_and_restart ->
                   [{nodes, [node1@localhost, node2@localhost]}];
               add_and_restart ->
                   [{nodes, [node1@localhost, node2@localhost]}];
               _ ->
                   [{nodes, [node1@localhost, node2@localhost, node3@localhost]}]
           end,
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
              , {?config(sched, Config),
                 [
                  
                 ]} }
            , verbose_final ] }
        , {node, node1@localhost}
        , {clock_limit, 60000}
        , {clock_offset, 1539105131938}
        , {aux_module, ?MODULE}
        , {aux_data, case os:getenv("SCOPED") of
                         false -> false;
                         [] -> false;
                         _ -> true
                     end}
        , stop_on_deadlock
        %% , trace_send, trace_receive, verbose_handle
        , {heartbeat, none}
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

prepare(Config) ->
    os:cmd("rm -r data"),
    os:cmd("mkdir data"),
    Nodes = ?config(nodes, Config),
    lists:foreach(
      fun (N) ->
              ?GH:bootstrap_remote(N),
              ok = rpc:call(N, application, set_env, [mnesia, dir, lists:flatten(io_lib:format("data/~w", [N]))])
      end, Nodes),
    ok = mnesia:create_schema(Nodes),
    ok.

prepare_and_start(Config) ->
    ok = prepare(Config),
    lists:foreach(
      fun (N) -> {ok, _} = rpc:call(N, application, ensure_all_started, [mnesia]) end, ?config(nodes, Config)),
    ok.

del_and_restart(Config) ->
    ok = prepare_and_start(Config),
    {atomic, ok} = mnesia:create_table(test_tab, [{disc_copies, [node1@localhost, node2@localhost]}]),
    ?G:set_flags([{tracing, true}]),
    Self = self(),

    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  ok = application:start(mnesia),
                  Self ! ok
          end),
    (fun Retry() ->
             case mnesia:del_table_copy(test_tab, node2@localhost) of
                 {atomic, ok} -> io:format(user, "del_table_copy ok~n", []), ok;
                 _R ->
                     io:format(user, "del_table_copy ~p~n", [_R]),
                     timer:sleep(100),
                     Retry()
             end
     end)(),
    receive ok -> ok end,
    timer:sleep(100),

    case mnesia:table_info(test_tab, disc_copies) of
        [node1@localhost] ->
            ok;
        _R ->
            io:format(user, "result mismatch: ~w~n", [rpc:multicall(?config(nodes, Config), mnesia, table_info, [test_tab, disc_copies])]),
            ?G:exit_with(result_mismatch)
    end,
    ok.

add_and_restart(Config) ->
    ok = prepare_and_start(Config),
    {atomic, ok} = mnesia:create_table(test_tab, [{disc_copies, [node1@localhost]}]),
    Self = self(),
    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  ok = application:start(mnesia),
                  Self ! ok
          end),
    (fun Retry() ->
             case mnesia:add_table_copy(test_tab, node2@localhost, disc_copies) of
                 {atomic, ok} ->
                     io:format(user, "add_table_copy ok~n", []), ok;
                 {aborted, {already_exists, _, _}} ->
                     io:format(user, "add_table_copy already exists?~n", []), ok;
                 _R ->
                     io:format(user, "add_table_copy ~p~n", [_R]),
                     timer:sleep(100),
                     Retry()
             end
     end)(),
    receive ok -> ok end,
    timer:sleep(100),

    case lists:member(node2@localhost, mnesia:table_info(test_tab, disc_copies)) of
        true ->
            ok;
        _R ->
            io:format(user, "result mismatch: ~w~n", [_R]),
            ?G:exit_with(result_mismatch)
    end,
    ok.

add_index_and_restart(Config) ->
    ok = prepare_and_start(Config),

    Self = self(),

    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, [node1@localhost, node2@localhost]}]),

    {atomic,ok} = mnesia:transaction(
                    fun () ->
                            mnesia:write(#test_record{ key = test_key, key2 = test_key2, value = test_value })
                    end),

    spawn(node2@localhost,
          fun () ->
                  (fun R() ->
                           case mnesia:add_table_index(test_record, #test_record.key2) of
                               {atomic, ok} -> ok;
                               _R -> io:format(user, "Retry with ~p~n", [_R]), timer:sleep(100), R()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  ok = application:start(mnesia),
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,

    case lists:sort(mnesia:table_info(test_record, index)) of
        [#test_record.key2] -> ok;
        _R -> io:format(user, "mismatched: ~p~n", [_R]), ?G:exit_with(mismatched)
    end,

    ok.

del_index_and_restart(Config) ->
    ok = prepare_and_start(Config),

    Self = self(),

    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {index, [#test_record.key2]},
                             {disc_copies, [node1@localhost, node2@localhost]}]),
    {atomic,ok} = mnesia:transaction(
                    fun () ->
                            mnesia:write(#test_record{ key = test_key, key2 = test_key2, value = test_value })
                    end),

    spawn(node2@localhost,
          fun () ->
                  (fun R() ->
                           case mnesia:del_table_index(test_record, #test_record.key2) of
                               {atomic, ok} -> ok;
                               _R -> io:format(user, "Retry with ~p~n", [_R]), timer:sleep(100), R()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  ok = application:start(mnesia),
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,

    case lists:sort(mnesia:table_info(test_record, index)) of
        [] -> ok;
        _R -> io:format(user, "mismatched: ~p~n", [_R]), ?G:exit_with(mismatched)
    end,

    ok.

%% bug found
add_index_and_restart_2(Config) ->
    ok = prepare_and_start(Config),

    Self = self(),

    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, [node1@localhost, node2@localhost]}]),

    {atomic,ok} = mnesia:transaction(
                    fun () ->
                            mnesia:write(#test_record{ key = test_key, key2 = test_key2, value = test_value })
                    end),

    spawn(node2@localhost,
          fun () ->
                  (fun R() ->
                           case mnesia:add_table_index(test_record, #test_record.key2) of
                               {atomic, ok} -> ok;
                               _R -> io:format(user, "Retry with ~p~n", [_R]), timer:sleep(100), R()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node3@localhost,
          fun () ->
                  application:stop(mnesia),
                  ok = application:start(mnesia),
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,

    case lists:sort(mnesia:table_info(test_record, index)) of
        [#test_record.key2] -> ok;
        _R -> io:format(user, "mismatched: ~p~n", [_R]), ?G:exit_with(mismatched)
    end,

    ok.

%% bug found
del_index_and_restart_2(Config) ->
    ok = prepare_and_start(Config),

    Self = self(),

    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {index, [#test_record.key2]},
                             {disc_copies, [node1@localhost, node2@localhost]}]),
    {atomic,ok} = mnesia:transaction(
                    fun () ->
                            mnesia:write(#test_record{ key = test_key, key2 = test_key2, value = test_value })
                    end),

    spawn(node3@localhost,
          fun () ->
                  (fun R() ->
                           case mnesia:del_table_index(test_record, #test_record.key2) of
                               {atomic, ok} -> ok;
                               _R -> io:format(user, "Retry with ~p~n", [_R]), timer:sleep(100), R()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  ok = application:start(mnesia),
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,

    case lists:sort(mnesia:table_info(test_record, index)) of
        [] -> ok;
        _R -> io:format(user, "mismatched: ~p~n", [_R]), ?G:exit_with(mismatched)
    end,

    ok.

%% no bug found yet
del_index_and_add_index(Config) ->
    ok = prepare_and_start(Config),

    Self = self(),

    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {index, [#test_record.key2]},
                             {disc_copies, [node1@localhost, node2@localhost]}]),
    {atomic,ok} = mnesia:transaction(
                    fun () ->
                            mnesia:write(#test_record{ key = test_key, key2 = test_key2, value = test_value })
                    end),

    spawn(node3@localhost,
          fun () ->
                  (fun R() ->
                           case mnesia:del_table_index(test_record, #test_record.key2) of
                               {atomic, ok} -> ok;
                               _R -> io:format(user, "Retry with ~p~n", [_R]), timer:sleep(100), R()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node2@localhost,
          fun () ->
                  (fun R() ->
                           case mnesia:add_table_index(test_record, #test_record.value) of
                               {atomic, ok} -> ok;
                               _R -> io:format(user, "Retry with ~p~n", [_R]), timer:sleep(100), R()
                           end
                   end)(),
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,

    case lists:sort(mnesia:table_info(test_record, index)) of
        [#test_record.value] -> ok;
        _R -> io:format(user, "mismatched: ~p~n", [_R]), ?G:exit_with(mismatched)
    end,

    ok.

%% no bug found yet
add_and_del(Config) ->
    ok = prepare_and_start(Config),
    {atomic, ok} = mnesia:create_table(test_tab, [{disc_copies, [node1@localhost, node2@localhost]}]),
    ?G:set_flags([{tracing, true}]),
    Self = self(),

    spawn(node3@localhost,
          fun () ->
                  (fun Retry() ->
                           case mnesia:del_table_copy(test_tab, node2@localhost) of
                               {atomic, ok} -> io:format(user, "del_table_copy ok~n", []), ok;
                               _R ->
                                   io:format(user, "del_table_copy ~p~n", [_R]),
                                   timer:sleep(100),
                                   Retry()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node2@localhost,
          fun () ->
                  (fun Retry() ->
                           case mnesia:add_table_copy(test_tab, node3@localhost, disc_copies) of
                               {atomic, ok} ->
                                   io:format(user, "add_table_copy ok~n", []), ok;
                               _R = {aborted, {already_exists, _, _}} ->
                                   io:format(user, "add_table_copy already exists ~p?~n", [_R]), ok;
                               _R ->
                                   io:format(user, "add_table_copy ~p~n", [_R]),
                                   timer:sleep(100),
                                   Retry()
                           end
                   end)(),
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,

    timer:sleep(100),

    case lists:sort(mnesia:table_info(test_tab, disc_copies)) of
        [node1@localhost, node3@localhost] ->
            ok;
        _R ->
            io:format(user, "result mismatch: ~w~n", [rpc:multicall(?config(nodes, Config), mnesia, table_info, [test_tab, disc_copies])]),
            ?G:exit_with(result_mismatch)
    end,
    ok.

%% no bug found yet
tx_write_and_restart(Config) ->
    ok = prepare_and_start(Config),
    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, ?config(nodes, Config)}]),

    Self = self(),
    spawn(node2@localhost,
          fun () ->
                  (fun Retry() ->
                           R = (catch mnesia:sync_transaction(
                                        fun () ->
                                                mnesia:write(#test_record{ key = test_key, value = test_value })
                                        end)),

                           case R of
                               {atomic, ok} -> io:format(user, "tx ok~n", []), ok;
                               _ -> io:format(user, "tx result ~w~n", [R]), timer:sleep(100), Retry()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node3@localhost,
          fun () ->
                  application:stop(mnesia),
                  R = application:start(mnesia),
                  case R of
                      ok -> ok;
                      _ ->
                          io:format(user, "application:start(mnesia) -> ~p~n", [R])
                  end,
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,

    timer:sleep(100),

    try
        case mnesia:transaction(fun () -> mnesia:read(test_record, test_key) end) of
            {atomic, [#test_record{key = test_key, value = test_value}]} ->
                ok;
            _R ->
                io:format(user, "result mismatch: ~w~n", [_R]),
                ?G:exit_with(result_mismatch)
        end
    catch
        C:E ->
            io:format(user, "get ~w:~w~n", [C, E]),
            ?G:exit_with(got_exception)
    end,
    ok.

tx_write_and_restart_2(Config) ->
    ok = prepare_and_start(Config),
    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, ?config(nodes, Config)}]),

    Self = self(),
    spawn(node2@localhost,
          fun () ->
                  (fun Retry() ->
                           R = (catch mnesia:transaction(
                                        fun () ->
                                                mnesia:write(#test_record{ key = test_key, value = test_value })
                                        end)),

                           case R of
                               {atomic, ok} -> io:format(user, "tx ok~n", []), ok;
                               _ -> io:format(user, "tx result ~w~n", [R]), timer:sleep(100), Retry()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  R = application:start(mnesia),
                  case R of
                      ok -> ok;
                      _ ->
                          io:format(user, "application:start(mnesia) -> ~p~n", [R])
                  end,
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,

    timer:sleep(100),

    try
        case mnesia:transaction(fun () -> mnesia:read(test_record, test_key) end) of
            {atomic, [#test_record{key = test_key, value = test_value}]} ->
                ok;
            _R ->
                io:format(user, "result mismatch: ~w~n", [_R]),
                ?G:exit_with(result_mismatch)
        end
    catch
        C:E ->
            io:format(user, "get ~w:~w~n", [C, E]),
            ?G:exit_with(got_exception)
    end,
    ok.

%% no bug found yet
tx_del_and_restart(Config) ->
    ok = prepare_and_start(Config),
    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, ?config(nodes, Config)}]),
    {atomic, ok} = mnesia:sync_transaction(
                     fun () ->
                             mnesia:write(#test_record{ key = test_key, value = test_value })
                     end),
    Self = self(),
    spawn(node2@localhost,
          fun () ->
                  (fun Retry() ->
                           R = (catch mnesia:transaction(
                                        fun () ->
                                                mnesia:delete({test_record, test_key})
                                        end)),

                           case R of
                               {atomic, ok} -> io:format(user, "tx ok~n", []), ok;
                               _ -> io:format(user, "tx result ~w~n", [R]), timer:sleep(100), Retry()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node3@localhost,
          fun () ->
                  application:stop(mnesia),
                  R = application:start(mnesia),
                  case R of
                      ok -> ok;
                      _ ->
                          io:format(user, "application:start(mnesia) -> ~p~n", [R])
                  end,
                  Self ! ok
          end),

    receive ok -> ok end,
    receive ok -> ok end,

    timer:sleep(100),

    try
        case mnesia:transaction(fun () -> mnesia:read(test_record, test_key) end) of
            {atomic, []} ->
                ok;
            _R ->
                io:format(user, "result mismatch: ~w~n", [_R]),
                ?G:exit_with(result_mismatch)
        end
    catch
        C:E ->
            io:format(user, "get ~w:~w~n", [C, E]),
            ?G:exit_with(got_exception)
    end,
    ok.

%% no bug found yet
add_and_del_and_tx(Config) ->
    ok = prepare_and_start(Config),
    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, [node1@localhost, node2@localhost]}]),
    ?G:set_flags([{tracing, true}]),
    Self = self(),

    spawn(node3@localhost,
          fun () ->
                  (fun Retry() ->
                           case mnesia:del_table_copy(test_record, node2@localhost) of
                               {atomic, ok} -> io:format(user, "del_table_copy ok~n", []), ok;
                               _R ->
                                   io:format(user, "del_table_copy ~p~n", [_R]),
                                   timer:sleep(100),
                                   Retry()
                           end
                   end)(),
                  Self ! ok
          end),
    spawn(node2@localhost,
          fun () ->
                  (fun Retry() ->
                           case mnesia:add_table_copy(test_record, node3@localhost, disc_copies) of
                               {atomic, ok} ->
                                   io:format(user, "add_table_copy ok~n", []), ok;
                               _R = {aborted, {already_exists, _, _}} ->
                                   io:format(user, "add_table_copy already exists ~p?~n", [_R]), ok;
                               _R ->
                                   io:format(user, "add_table_copy ~p~n", [_R]),
                                   timer:sleep(100),
                                   Retry()
                           end
                   end)(),
                  Self ! ok
          end),
    (fun Retry() ->
             R = (catch mnesia:transaction(
                          fun () ->
                                  mnesia:write(#test_record{ key = test_key, value = test_value })
                          end)),

             case R of
                 {atomic, ok} -> io:format(user, "tx ok~n", []), ok;
                 _ -> io:format(user, "tx result ~w~n", [R]), timer:sleep(100), Retry()
             end
     end)(),
    receive ok -> ok end,
    receive ok -> ok end,

    timer:sleep(100),

    try
        case rpc:call(node2@localhost, mnesia, transaction, [fun () -> mnesia:read(test_record, test_key) end]) of
            {atomic, [#test_record{key = test_key, value = test_value}]} ->
                ok;
            _R ->
                io:format(user, "result mismatch: ~w~n", [_R]),
                ?G:exit_with(result_mismatch)
        end
    catch
        C:E ->
            io:format(user, "get ~w:~w~n", [C, E]),
            ?G:exit_with(got_exception)
    end,
    ok.

%% no bug found yet
create_and_wait_table(Config) ->
    ok = prepare_and_start(Config),

    Self = self(),

    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_only_copies, [node1@localhost, node2@localhost]}]),
    {atomic,ok} = mnesia:transaction(
                    fun () ->
                            mnesia:write(#test_record{ key = test_key, value = test_value })
                    end),

    spawn(node3@localhost,
          fun () ->
                  {atomic, ok} = mnesia:add_table_copy(test_record, node3@localhost, disc_only_copies),
                  Self ! ok
          end),
    spawn(node3@localhost,
          fun () ->
                  ok = mnesia:wait_for_tables([test_record], infinity),
                  Self ! ok
          end),

    receive ok -> ok end,
    receive ok -> ok end,

    ok.

%% no bug found yet
add_copy_and_create_index(Config) ->
    ok = prepare_and_start(Config),

    Self = self(),

    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, [node1@localhost, node2@localhost]}]),
    {atomic,ok} = mnesia:transaction(
                    fun () ->
                            mnesia:write(#test_record{ key = test_key, value = test_value })
                    end),

    spawn(node2@localhost,
          fun () ->
                  {atomic,ok} = mnesia:add_table_copy(test_record, node3@localhost, disc_copies),
                  Self ! ok
          end),
    spawn(node3@localhost,
          fun () ->
                  {atomic, ok} = mnesia:add_table_index(test_record, #test_record.value),
                  Self ! ok
          end),
    {atomic, ok} = mnesia:add_table_index(test_record, #test_record.value2),
    receive ok -> ok end,
    receive ok -> ok end,

    case lists:sort(mnesia:table_info(test_record, index)) of
        [#test_record.value, #test_record.value2] -> ok;
        _R -> io:format(user, "mismatched: ~p~n", [_R]), ?G:exit_with(mismatched)
    end,

    ok.
