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
        , {testcase, try_getenv("TESTCASE", fun list_to_atom/1, del_copy_and_restart)}
        , {dump, try_getenv("DUMP", fun (I) -> I =/= "" end, false)}
        , {pred, try_getenv("PRED", fun list_to_atom/1, no)}
        ],
    Config = Config0
        ++ case ?config(testcase, Config0) of
               del_copy_and_restart ->
                   [{nodes, [node1@localhost, node2@localhost]}];
               add_copy_and_restart ->
                   [{nodes, [node1@localhost, node2@localhost]}];
               _ ->
                   [{nodes, [node1@localhost, node2@localhost, node3@localhost]}]
           end,
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
        %% , trace_send, trace_receive
        %% , verbose_handle
        %% , trace_from_start
        , {heartbeat, none}
        , {execution_limit, 20000}
        , {undet_timeout, 50}
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

t_sandbox_entry(Config) ->
    FNAME = ?config(testcase, Config),
    apply(?MODULE, FNAME, [Config]),
    ?G:exit_with(success).

prepare(Config) ->
    os:cmd("rm -r data MnesiaCore.*"),
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

%% bug found
del_copy_and_restart(Config) ->
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

%% bug found
add_copy_and_restart(Config) ->
    ok = prepare_and_start(Config),
    {atomic, ok} = mnesia:create_table(test_tab, [{disc_copies, [node1@localhost]}]),
    ?G:set_flags([{tracing, true}]),
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

%% trivially deadlock
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

%% trivially deadlock
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
add_copy_and_del_copy(Config) ->
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
add_copy_and_del_copy_and_tx(Config) ->
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

%% related to http://erlang.org/pipermail/erlang-questions/2013-March/072880.html
%% cannot reproduce on OTP-20
dirty_read_index(Config) ->
    ok = prepare_and_start(Config),
    Self = self(),
    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, [node2@localhost, node3@localhost]}]),
    {atomic, ok} = mnesia:add_table_index(test_record, #test_record.key2),
    (fun R (0) -> ok;
         R (I) -> ok = mnesia:dirty_write(#test_record{key = I, key2 = i}), R(I - 1)
     end)(10),
    ok = mnesia:dirty_write(#test_record{key = 0, key2 = j}),
    spawn(fun () ->
                  {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(#test_record{key = 0, key2 = j, value = x}) end)
          end),
    case mnesia:dirty_match_object(#test_record{key = 0, key2 = j, _ = '_'}) of
        [] -> ?G:exit_with(empty_match);
        _R when length(_R) =:= 1 -> ok;
        _R -> ?G:exit_with(unexpected)
    end,
    ok.

retry_until(Fun, Expected) ->
    case Fun() of
        Expected ->
            ok;
        _R ->
            io:format(user, "retry with ~p~n", [_R]),
            timer:sleep(100),
            retry_until(Fun, Expected)
    end.

-define(F(Exp), (fun () -> Exp end)).

multi_tx(Config) ->
    ok = prepare_and_start(Config),
    Self = self(),
    {atomic, ok} =
        mnesia:create_table(test_record,
                            [{attributes, record_info(fields, test_record)},
                             {disc_copies, [node2@localhost, node3@localhost]}]),

    {atomic, ok} = mnesia:transaction(
                     fun () ->
                             mnesia:write(#test_record{key = a, value = x})
                     end),
    {atomic, ok} = mnesia:transaction(
                     fun () ->
                             mnesia:write(#test_record{key = b, value = y})
                     end),
    {atomic, ok} = mnesia:transaction(
                     fun () ->
                             mnesia:write(#test_record{key = c, value = z})
                     end),
    Self = self(),
    spawn(?F(retry_until(
               ?F(mnesia:transaction(
                    ?F(begin
                           mnesia:write(#test_record{key = a, value = x1}),
                           mnesia:write(#test_record{key = b, value = y1}),
                           mnesia:write(#test_record{key = c, value = z1}),
                           ok
                       end))), {atomic, ok}))),
    spawn(node2@localhost,
          ?F(retry_until(
               ?F(mnesia:transaction(
                    ?F(begin
                           mnesia:write(#test_record{key = c, value = z2}),
                           mnesia:write(#test_record{key = a, value = x2}),
                           mnesia:write(#test_record{key = b, value = y2}),
                           ok
                       end))), {atomic, ok}))),
    spawn(node3@localhost,
          ?F(retry_until(
               ?F(mnesia:transaction(
                    ?F(begin
                           mnesia:write(#test_record{key = b, value = y3}),
                           mnesia:write(#test_record{key = c, value = z3}),
                           mnesia:write(#test_record{key = a, value = x3}),
                           ok
                       end))), {atomic, ok}))),
    case mnesia:transaction(
           ?F(begin
                  [#test_record{value = V1}] = mnesia:read(test_record, a),
                  [#test_record{value = V2}] = mnesia:read(test_record, b),
                  [#test_record{value = V3}] = mnesia:read(test_record, c),
                  {V1, V2, V3}
              end))
    of
        {atomic, {x, y, z}} -> ok;
        {atomic, {x1, y1, z1}} -> ok;
        {atomic, {x2, y2, z2}} -> ok;
        {atomic, {x3, y3, z3}} -> ok;
        _R ->
            io:format(user, "unexpected - ~p~n", [_R]),
            ?G:exit_with(unexpected)
    end,

    ok.
