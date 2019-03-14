-module(protocol_driver).

-include("protocol.hrl").

-export([start/2]).

-record(protocol_state,
        { mod   :: module()
        , state :: term()
        }).

entry(Mod, Opts) ->
    State = #protocol_state{ mod = Mod
                    , state = Mod:init(Opts)
                    },
    loop(State).

loop(State) ->
    receive
        M ->
            loop(handle_input(M, State))
    end.

handle_input(Input, #protocol_state{mod = Mod, state = State} = PState) ->
    case Mod:handle(Input, State) of
        {Actions, State1} ->
            handle_actions(Actions),
            PState#protocol_state{state = State1};
        {Actions, State1, NextInput} ->
            handle_actions(Actions),
            handle_input(NextInput, PState#protocol_state{state = State1})
    end.

handle_actions([]) ->
    ok;
handle_actions([?send(To, Content) | Tail]) ->
    %% io:format(user, "~w == ~w => ~w~n", [self(), Content, To]), 
    To ! Content,
    handle_actions(Tail).

start(Mod, Opts) ->
    spawn(fun () ->
                  entry(Mod, Opts)
          end).
