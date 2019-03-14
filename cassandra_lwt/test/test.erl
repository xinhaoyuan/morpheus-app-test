-module(test).

-include("../src/protocol.hrl").
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

%% is_racing(#fd_delay_req{to = global}, #fd_delay_req{}) ->
%%     false;
%% is_racing(#fd_delay_req{}, #fd_delay_req{to = global}) ->
%%     false;
%% is_racing(#fd_delay_req{to = To, data = #{req := R1}}, #fd_delay_req{to = To, data = #{req := R2}}) ->
%%     case {R1, R2} of
%%         %% {{process_send, _, _, {msg_commit, _, _, _}}, {process_send, _, _, {msg_commit, _, _, _}}} ->
%%         %%     io:format(user, "!!! ~w ~w~n", [R1, R2]),
%%         %%     true;
%%         %% {{process_send, _, _, {msg_propose, _, _, _}}, {process_send, _, _, {msg_prepare, _, _}}} ->
%%         %%     io:format(user, "!!! ~w ~w~n", [R1, R2]),
%%         %%     true;
%%         %% Only this one improve the probability
%%         %% {{process_send, _, _, {msg_commit, _, _, _}}, {process_send, _, _, {msg_propose, _, _, _}}} ->
%%         %%     io:format(user, "!!! ~w ~w~n", [R1, R2]),
%%         %%     true;
%%         %% {{process_send, _, _, {msg_propose, _, _, _}}, {process_send, _, _, {msg_commit, _, _, _}}} ->
%%         %%     io:format(user, "!!! ~w ~w~n", [R1, R2]),
%%         %%     true;
%%         _ ->
%%             false
%%     end;
%% is_racing(#fd_delay_req{}, #fd_delay_req{}) ->
%%     false.

%% ?MORPHEUS_CB_ANNOTATE_SCHED_DATA_FN(_, Data, From, Req) ->
%%     D1 = Data#{req => Req}, 
%%     case Req of
%%         ?cci_send_msg(_, _, Msg) ->
%%             case is_tuple(Msg) andalso size(Msg) >= 3 andalso element(3, Msg) of
%%                 {Num, _} ->
%%                     io:format(user, "!!! annotate ~w with ~w~n", [Req, Num]), 
%%                     D1#{weight => Num + 1};
%%                 _ ->
%%                     io:format(user, "!!! ??? ~w~n", [Req]), 
%%                     D1
%%             end;
%%         _ ->
%%             D1
%%     end.

%% is_racing(_, _) ->
%%     false.

test_entry() ->
    Config =
        [ {sched, try_getenv("SCHED", fun list_to_atom/1, basicpos)}
        , {acc_filename, try_getenv("ACC_FILENAME", fun (I) -> I end, "acc.dat")}
        , {dump, try_getenv("DUMP", fun (I) -> I =/= "" end, false)}
        , {check_consistency, try_getenv("CHECK_CONSISTENCY", fun list_to_atom/1, false)}
        , {check_duplicate, try_getenv("CHECK_DUPLICATE", fun list_to_atom/1, false)}
        , {testcase, try_getenv("TESTCASE", fun list_to_atom/1, test_v2)}
        ],
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
                  %% {seed, {exrop,[69234962250945757|600092897920190]}}
                  %% {is_racing_fun, fun is_racing/2}
                 ]} }
            , verbose_final ] }
        , {node, node1@localhost}
        , {clock_limit, 60000}
        , {clock_offset, 1539105131938}
        , {aux_module, ?MODULE}
        , stop_on_deadlock
        %% , trace_send, trace_receive, trace_from_start
        , {heartbeat, none}
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
    TC = ?config(testcase, Config),
    apply(?MODULE, TC, [Config]),
    ?G:exit_with(success).

submit(Client, Cmd) ->
    Ref = make_ref(),
    Client ! #submit_req{from = self(), ref = Ref, command = Cmd},
    receive
        #submit_resp{ref = Ref} ->
            ok
    after
        100 ->
            timeout
    end.

get_commit_history(Server) ->
    Ref = make_ref(),
    Server ! #get_commit_history_req{from = self(), ref = Ref},
    receive
        #get_commit_history_resp{ref = Ref, history = H} ->
            H
    end.

dedup(L) ->
    {_, R} =
        lists:foldr(
          fun (H, {Prev, R}) when H =:= Prev, R =/= [] ->
                  {Prev, R};
              (H, {Prev, R}) ->
                  {H, [H | R]}
          end, {undefined, []}, L),
    R.

proxy(Master, Ref) ->
    receive
        {Ref, To, Msg} ->
            To ! Msg,
            proxy(Master, Ref);
        {Ref, exit} ->
            ok;
        Other ->
            Master ! {Ref, Other},
            proxy(Master, Ref)
    end.    

c6023(Config) ->
    Servers = lwt:start_v1_servers(3),
    Items = [x,y,z],
    Self = self(),
    %% Clients =
    %%     lists:foldl(
    %%       fun (I, Acc) ->
    %%               [{lwt:start_v1_client(Servers), I} | Acc]
    %%       end, [], Items),
    %% lists:foreach(
    %%   fun ({C, I}) ->
    %%           spawn(fun() ->
    %%                         submit(C, I),
    %%                         Self ! ok
    %%                 end)
    %%   end, Clients),
    %% lists:foreach(
    %%   fun (_) ->
    %%           receive ok -> ok end
    %%   end, Clients),
    C1 = lwt:start_v1_client(Servers),
    C2 = lwt:start_v1_client(Servers),
    C3 = lwt:start_v1_client(Servers),
    spawn(fun () ->
                  submit(C1, x),
                  submit(C2, y),
                  Self ! ok
          end),
    spawn(fun () ->
                  submit(C3, z),
                  Self ! ok
          end),
    receive ok -> ok end,
    receive ok -> ok end,
    lists:foreach(
      fun (S) ->
              H = [Y || {X, Y} <- get_commit_history(S)],
              HDedup = dedup(H),
              io:format(user, "history ~w ~w ~w~n", [S, H, HDedup]),
              true = length(HDedup) =:= length(lists:usort(HDedup))
      end, Servers),
    ok.

test_v2(Config) ->
    Servers = [SA | _] = lwt:start_v2_servers(3),
    Items = [x, y, z],
    Clients =
        lists:foldl(
          fun (I, Acc) ->
                  [{lwt:start_v2_client(Servers), I} | Acc]
          end, [], Items),
    Self = self(),
    lists:foreach(
      fun ({C, I}) ->
              spawn(fun() ->
                            submit(C, I),
                            Self ! ok
                    end)
      end, Clients),
    lists:foreach(
      fun (_) ->
              receive ok -> ok end
      end, Clients),
    H = [Y || {X, Y} <- get_commit_history(SA)],
    HDedup = dedup(H),
    io:format(user, "~w ~w~n", [H, HDedup]),
    case ?config(check_consistency, Config) of
        true ->
            lists:foreach(
              fun (S) ->
                      HDedup = dedup([Y || {X, Y} <- get_commit_history(S)])
              end, Servers -- [SA]),
            ok;
        false ->
            ok
    end,
    case ?config(check_duplicate, Config) of
        true ->
            true = length(HDedup) =:= length(lists:usort(HDedup));
        false ->
            ok
    end,
    ok.
