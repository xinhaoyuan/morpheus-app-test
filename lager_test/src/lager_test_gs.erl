-module(lager_test_gs).
-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
               }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call({example_call}, _From, State) ->
    lager:info("Got an example call", []),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
