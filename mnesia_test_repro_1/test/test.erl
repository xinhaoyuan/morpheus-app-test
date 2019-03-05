-module(test).

-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 20, ?_test( test_entry() )}.

spawn_node(NodeName) ->
    {ok, Node} = slave:start("localhost", NodeName),
    ok = rpc:call(Node, code, add_paths, [code:get_path()]),
    Node.

test_entry() ->
    os:cmd("epmd -daemon"),
    {ok, _} = net_kernel:start([node1@localhost, shortnames]),
    Node1 = node(),
    Node2 = spawn_node(node2),
    Nodes = [Node1, Node2],

    Self = self(),

    os:cmd("rm -r data"),
    os:cmd("mkdir data"),
    lists:foreach(
      fun (N) ->
              ok = rpc:call(N, application, set_env, [mnesia, dir, lists:flatten(io_lib:format("data/~w", [N]))])
      end, Nodes),
    ok = mnesia:create_schema(Nodes),
    lists:foreach(
      fun (N) -> {ok, _} = rpc:call(N, application, ensure_all_started, [mnesia]) end, Nodes),

    {atomic, ok} = mnesia:create_table(test_tab, [{disc_copies, [node1@localhost]}]),

    timer:sleep(1000),

    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  Self ! ok,
                  io:format(user, "restart mnesia~n", []),
                  ok = application:start(mnesia),
                  ok
          end),
    receive ok -> ok end,

    (fun R() ->
             case mnesia:add_table_copy(test_tab, node2@localhost, disc_copies) of
                 {atomic, ok} -> ok;
                 _R -> io:format(user, "try with reason ~p~n", [_R]), timer:sleep(50), R()
             end
     end)(),
    timer:sleep(1000),

    ok.

-endif.
