-module(lwt).

-export([ start_v1_client/1
        , start_v1_servers/1
        , start_v2_client/1
        , start_v2_servers/1]).

start_v2_client(Servers) ->
    protocol_driver:start(client, #{version => v2, servers => Servers}).

start_v2_server() ->
    protocol_driver:start(server, #{version => v2}).

start_v2_servers(Count) ->
    (fun R(Acc, 0) -> Acc;
         R(Acc, C) ->
             R([start_v2_server() | Acc], C - 1)
     end)([], Count).

start_v1_client(Servers) ->
    protocol_driver:start(client, #{version => v1, servers => Servers}).

start_v1_server() ->
    protocol_driver:start(server, #{version => v1}).

start_v1_servers(Count) ->
    (fun R(Acc, 0) -> Acc;
         R(Acc, C) ->
             R([start_v1_server() | Acc], C - 1)
     end)([], Count).
