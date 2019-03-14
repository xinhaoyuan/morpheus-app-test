-module(server).

-include("protocol.hrl").

-export([init/1, handle/2]).

-record(server,
        { self            :: server_id
        , version         :: v1 | v2
        , promised_ballot :: undefined | ballot_id()
        , accepted        :: undefined | {ballot_id(), item()}
        , committed       :: [{ballot_id(), item()}]
        }).

%% In v1, accepted ballots are not used in the protocol. Instead, promised ballots are used.

init(Opts) ->
    #server{ self = maps:get(self, Opts, self())
           , version = maps:get(version, Opts, v2)
           , promised_ballot = undefined
           , accepted = undefined
           , committed = []
           }.

handle(#msg_prepare{from = From, ballot = Ballot},
       #server{ self = Self
              , version = Version
              , promised_ballot = PromisedBallot
              , accepted = Accepted
              , committed = Committed
              } = State) ->
    case PromisedBallot =:= undefined orelse PromisedBallot < Ballot of
        true ->
            Msg0 =
                case Version of
                    v2 ->
                        #msg_promise{ from = Self
                                    , ballot = Ballot
                                    , accepted = Accepted
                                    };
                    v1 ->
                        case Accepted of
                            undefined ->
                                #msg_promise{ from = Self
                                            , ballot = Ballot
                                            , accepted = undefined
                                            };
                            {_, AcceptedItem} ->
                                #msg_promise{ from = Self
                                            , ballot = Ballot
                                            , accepted = {PromisedBallot, AcceptedItem}
                                            }
                        end
                end,
            Msg =
                case Committed of
                    [H | T] ->
                        Msg0#msg_promise{committed = H};
                    [] ->
                        Msg0
                end,
            {[?send(From, Msg)],
             State#server{promised_ballot = Ballot}};
        false ->
            {[?send(From, #msg_prepare_reject{from = Self, ballot = Ballot, rejected_ballot = PromisedBallot})], State}
    end;
handle(#msg_propose{from = From, ballot = Ballot, item = Item},
       #server{ self = Self
              , version = Version
              , promised_ballot = PromisedBallot
              , accepted = Accepted
              } = State) ->
    ToAccept =
        case Version of
            v1 ->
                (PromisedBallot =:= undefined orelse PromisedBallot =< Ballot);
            v2 ->
                (PromisedBallot =:= undefined orelse PromisedBallot =< Ballot)
                    andalso (Accepted =:= undefined orelse element(1, Accepted) < Ballot)
        end,
    case ToAccept of
        true ->
            {[?send(From, #msg_accepted{from = Self,
                                        ballot = Ballot})],
             case Version of
                 v1 ->
                     State#server{promised_ballot = Ballot, accepted = {Ballot, Item}};
                 v2 ->
                     State#server{accepted = {Ballot, Item}}
             end};
        false ->
            {[], State}
    end;
handle(#msg_commit{from = From, ballot = Ballot, item = Item},
       #server{ self = Self
              , version = Version
              , promised_ballot = PromisedBallot
              , accepted = Accepted
              , committed = Committed
              } = Server) ->
    Committed1 = commit(Ballot, Item, Committed),
    Server1 =
        case Version of
            v1 ->
                case Accepted =:= undefined orelse PromisedBallot =< Ballot of
                    true ->
                        Server#server{accepted = undefined, committed = Committed1};
                    false ->
                        Server#server{committed = Committed1}
                end;
            v2 ->
                case Accepted =:= undefined orelse element(1, Accepted) =< Ballot of
                    true ->
                        Server#server{accepted = undefined, committed = Committed1};
                    false ->
                        Server#server{committed = Committed1}
                end
        end,
    {[?send(From, #msg_ack{from = Self, ballot = Ballot})], Server1};
handle(#get_commit_history_req{from = From, ref = Ref},
       #server{committed = Committed} = State) ->
    {[?send(From, #get_commit_history_resp{ref = Ref, history = Committed})], State}.

commit(Ballot, Item, []) ->
    [{Ballot, Item}];
commit(Ballot, Item, [{HeadBallot, Head} | Tail]) when Ballot =< HeadBallot ->
    [{HeadBallot, Head} | Tail];
commit(Ballot, Item, [{HeadBallot, Head} | Tail]) ->
    [{Ballot, Item}, {HeadBallot, Head} | Tail].
