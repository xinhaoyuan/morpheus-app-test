%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2011-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(mirrored_supervisor_test).

-behaviour(mirrored_supervisor).

-compile(export_all).

-define(MS,     mirrored_supervisor).
-define(SERVER, mirrored_supervisor_test_gs).

-define(config(Key, Config), proplists:get_value(Key, Config)).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

-define(GH, morpheus_guest_helper).
-define(G, morpheus_guest).

test_entry() ->
    Config = [ %%
               {node_id,  {testcase_node1, node()}}
             , {node_id2, {testcase_node2, node()}}
             , {node_id3, {testcase_node3, node()}}
             , {cluster_id, <<"cluster">>}
             , {uid, <<"node_uid">>}
             , {priv_dir, "/tmp/rabbit"}
             ],
    init_per_suite(Config),
    leave_while_join(Config),

    ok.

init_per_suite(Config) ->
    ok = application:set_env(mnesia, dir, ?config(priv_dir, Config)),
    ok = application:start(mnesia),
    lists:foreach(
      fun ({Tab, TabDef}) ->
              TabDef1 = proplists:delete(match, TabDef),
              case mnesia:create_table(Tab, TabDef1) of
                  {atomic, ok} ->
                      ok;
                  {aborted, Reason} ->
                      throw({error,
                          {table_creation_failed, Tab, TabDef1, Reason}})
              end
      end, mirrored_supervisor:table_definitions()),
    Config.

end_per_suite(Config) ->
    ok = application:stop(mnesia),
    Config.

%% ---------------------------------------------------------------------------
%% Functional tests
%% ---------------------------------------------------------------------------

leave_while_join(_Config) ->
    passed =
        with_sups(
          fun ([A, B]) ->
                  {ok, _} = ?MS:start_child(a, childspec(worker)),
                  Counter = get(counter),
                  {_, M} = spawn_monitor(
                             fun() ->
                                     io:format(user, "killing sup b~n", []),
                                     timer:sleep(250),
                                     gen_server:stop(B),
                                     io:format(user ,"killed sup b~n", [])
                             end),
                  io:format(user, "joining sup c~n", []),
                  {ok, C} = start_sup(c),
                  io:format(user, "joined sup c joined~n", []),
                  gen_server:stop(c),
                  lists:foreach(
                    fun (M) ->
                            receive {'DOWN', M, _, _, _} -> ok end
                    end, [M]),
                  ok
          end, [a, b]).

%% ---------------------------------------------------------------------------

with_sups(Fun, Sups) ->
    inc_group(),
    Pids = [begin {ok, Pid} = start_sup(Sup), Pid end || Sup <- Sups],
    Fun(Pids),
    [kill(Pid) || Pid <- Pids, is_process_alive(Pid)],
    timer:sleep(500),
    passed.

start_sup(Spec) ->
    start_sup(Spec, group).

start_sup({Name, ChildSpecs}, Group) ->
    {ok, Pid} = start_sup0(Name, get_group(Group), ChildSpecs),
    %% We are not a supervisor, when we kill the supervisor we do not
    %% want to die!
    unlink(Pid),
    {ok, Pid};

start_sup(Name, Group) ->
    start_sup({Name, []}, Group).

start_sup0(anon, Group, ChildSpecs) ->
    ?MS:start_link(Group, fun tx_fun/1, ?MODULE,
                   {one_for_one, ChildSpecs});

start_sup0(Name, Group, ChildSpecs) ->
    ?MS:start_link({local, Name}, Group, fun tx_fun/1, ?MODULE,
                   {one_for_one, ChildSpecs}).

childspec(Id) ->
    {Id,{?SERVER, start_link, [Id]}, transient, 16#ffffffff, worker, [?MODULE]}.

pid_of(Id) ->
    {received, Pid, ping} = call(Id, ping),
    Pid.

tx_fun(Fun) ->
    case mnesia:sync_transaction(Fun) of
        {atomic,  Result}         -> Result;
        {aborted, Reason}         -> throw({error, Reason})
    end.

inc_group() ->
    Count = case get(counter) of
                undefined -> 0;
                C         -> C
            end + 1,
    put(counter, Count).

get_group(Group) ->
    {Group, get(counter)}.

call(Id, Msg) -> call(Id, Msg, 10*1000, 100).

call(Id, Msg, 0, _Decr) ->
    exit({timeout_waiting_for_server, {Id, Msg}, erlang:get_stacktrace()});

call(Id, Msg, MaxDelay, Decr) ->
    try
        gen_server:call(Id, Msg, infinity)
    catch exit:_ -> timer:sleep(Decr),
                    call(Id, Msg, MaxDelay - Decr, Decr)
    end.

kill(Pid) -> kill(Pid, []).
kill(Pid, Wait) when is_pid(Wait) -> kill(Pid, [Wait]);
kill(Pid, Waits) ->
    erlang:monitor(process, Pid),
    [erlang:monitor(process, P) || P <- Waits],
    exit(Pid, bang),
    kill_wait(Pid),
    [kill_wait(P) || P <- Waits].

kill_registered(Pid, Child) ->
    {registered_name, Name} = erlang:process_info(Child, registered_name),
    kill(Pid, Child),
    false = (Child =:= whereis(Name)),
    ok.

kill_wait(Pid) ->
    receive
        {'DOWN', _Ref, process, Pid, _Reason} ->
            ok
    end.

%% ---------------------------------------------------------------------------

init({fake_strategy_for_ignore, _ChildSpecs}) ->
    ignore;

init({Strategy, ChildSpecs}) ->
    {ok, {{Strategy, 0, 1}, ChildSpecs}}.
