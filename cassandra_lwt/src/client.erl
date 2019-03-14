-module(client).

-include("protocol.hrl").

-record(prepare,
        { votes                 :: dict:dict(server_id(), ballot_id()) %% server => most recent commit ballot
        , rejects               :: sets:set(server_id())
        , last_accepted_ballot  :: undefined | ballot_id()
        , last_accepted         :: undefined | item()
        , last_committed_ballot :: undefined | ballot_id()
        , last_committed        :: undefined | item()
        , last_rejected_ballot  :: undefined | ballot_id()
        }).

-record(propose,
        { fix                   :: boolean()
        , votes                 :: sets:set(server_id())
        , proposal              :: item()
        }).

-record(commit,
        { fix                   :: boolean()
        , commit_ballot         :: ballot_id()
        , waiting_acks          :: sets:set(server_id())
        }).

-record(client,
        { self                  :: client_id()
        , version               :: v1 | v2
        , servers               :: [server_id()]
        , ballot_num            :: ballot_num()
        , state                 :: idle | #prepare{} | #propose{} | #commit{}
        , pending_command       :: undefined | item()
        , pending_from          :: undefined | {pid(), reference()}
        }).

-export([init/1, handle/2]).

init(Opts) ->
    #client{ self = maps:get(self, Opts, self())
           , version = maps:get(version, Opts, v2)
           , servers = maps:get(servers, Opts)
           , ballot_num = 0
           , state = idle
           , pending_command = undefined
           , pending_from = undefined
           }.

handle(#submit_req{from = From, ref = Ref, command = Cmd},
       #client{state = idle} = State) ->
    {[], State#client{pending_command = Cmd, pending_from = {From, Ref}}, start_prepare};
handle(start_prepare,
       #client
       { self = Self
       , servers = Servers
       , ballot_num = BallotNum
       } = State) ->
    {[?send(S, #msg_prepare{from = Self, ballot = {BallotNum, Self}}) || S <- Servers],
     State#client{state = #prepare{ votes = dict:new()
                                  , rejects = sets:new()
                                  , last_accepted_ballot = undefined
                                  , last_accepted = undefined
                                  , last_committed_ballot = undefined
                                  , last_committed = undefined
                                  , last_rejected_ballot = undefined
                                  }}};
handle(#msg_prepare_reject
       { from = From
       , ballot = {BallotNum, Self}
       , rejected_ballot = RejBallot
       },
       #client
       { self = Self
       , version = Version
       , servers = Servers
       , ballot_num = BallotNum
       , state = #prepare
         { rejects = Rejects
         , last_rejected_ballot = LRBallot
         } = PPState
       } = State) ->
    case lists:member(From, Servers) andalso not sets:is_element(From, Rejects) of
        true ->
            Rejects1 = sets:add_element(From, Rejects),
            LRBallot1 =
                case LRBallot =:= undefined orelse LRBallot < RejBallot of
                    true ->
                        RejBallot;
                    false ->
                        LRBallot
                end,
            case sets:size(Rejects1) * 2 > length(Servers) of
                true ->
                    {LRBallotNum, _} = LRBallot1,
                    case Version of
                        v1 ->
                            %% do not attempt to retry
                            {[], State#client{state = idle, pending_command = undefined, pending_from = undefined}};
                        v2 ->
                            {[], State#client{state = idle, ballot_num = LRBallotNum + 1}, start_prepare}
                    end; 
                false ->
                    {[], State#client{state = PPState#prepare{rejects = Rejects1, last_rejected_ballot = LRBallot1}}}
            end;
        false ->
            {[], State}
    end;
%% handle promise
handle(#msg_promise
       { from = From
       , ballot = {BallotNum, Self}
       , accepted = Accepted
       , committed = Committed
       },
       #client
       { self = Self
       , servers = Servers
       , ballot_num  = BallotNum
       , state = #prepare
         { votes = Votes
         , last_accepted_ballot = LABallot
         , last_accepted = LA
         , last_committed_ballot = LCBallot
         , last_committed = LC
         } = PPState
       } = State) ->
    case lists:member(From, Servers) andalso not dict:is_key(From, Votes) of
        true ->
            Votes1 = dict:store(From, case Committed of undefined -> undefined; _ -> element(1, Committed) end, Votes),
            {LABallot1, LA1} =
                case Accepted =/= undefined andalso (LABallot =:= undefined orelse LABallot < element(1, Accepted)) of
                    true ->
                        Accepted;
                    false ->
                        {LABallot, LA}
                end,
            {LCBallot1, LC1} =
                case Committed =/= undefined andalso (LCBallot =:= undefined orelse LCBallot < element(1, Committed)) of
                    true ->
                        Committed;
                    false ->
                        {LCBallot, LC}
                end,
            case dict:size(Votes1) * 2 > length(Servers) of
                true ->
                    case LABallot1 =/= undefined andalso (LCBallot1 =:= undefined orelse LABallot1 > LCBallot1) of
                        true ->
                            %% fix for in-progress item
                            {[?send(S,
                                    #msg_propose{ from = Self
                                                , ballot = {BallotNum, Self}
                                                , item = LA1})
                              || S <- Servers],
                             State#client
                             { state = #propose
                               { fix = true
                               , votes = sets:new()
                               , proposal = LA1
                               }
                             }};
                        _ ->
                            FixCommitServers =
                                dict:fold(
                                  fun (S, C, Acc) ->
                                          case LCBallot1 =/= undefined andalso (C =:= undefined orelse C < LCBallot1) of
                                              true ->
                                                  [S | Acc];
                                              false ->
                                                  Acc
                                          end
                                  end, [], Votes1),
                            case FixCommitServers of
                                [] ->
                                    %% propose our command
                                    {[?send(S,
                                            #msg_propose{ from = Self
                                                        , ballot = {BallotNum, Self}
                                                        , item = State#client.pending_command})
                                      || S <- Servers],
                                     State#client
                                     { state = #propose
                                       { fix = false
                                       , votes = sets:new()
                                       , proposal = State#client.pending_command
                                       }
                                     }};
                                _ ->
                                    %% fix for missing commits
                                    {[?send(S,
                                            #msg_commit{ from = Self
                                                       , ballot = LCBallot1
                                                       , item = LC1})
                                      || S <- FixCommitServers],
                                     State#client
                                     { state = #commit
                                       { fix = true
                                       , commit_ballot = LCBallot1
                                       , waiting_acks = sets:from_list(FixCommitServers)
                                       }
                                     }}
                            end
                    end;
                false ->
                    {[],
                     State#client
                     { state = PPState#prepare
                       { votes = Votes1
                       , last_accepted_ballot = LABallot1
                       , last_accepted = LA1
                       , last_committed_ballot = LCBallot1
                       , last_committed = LC1
                       }
                     }
                    }
            end;
        false ->
            %% Voted or unrecognized server
            {[], State}
    end;
%% handling accepted
handle(#msg_accepted
       { from = From
       , ballot = {BallotNum, Self}
       },
       #client
       { self = Self
       , servers = Servers
       , ballot_num = BallotNum
       , state = #propose
         { fix = Fix
         , votes = Votes
         , proposal = Proposal
         } = PPState
       } = State) ->
    case lists:member(From, Servers) andalso not sets:is_element(From, Votes) of
        true ->
            Votes1 = sets:add_element(From, Votes),
            case sets:size(Votes1) * 2 > length(Servers) of
                true ->
                    {[?send(S, #msg_commit{from = Self, ballot = {BallotNum, Self}, item = Proposal})
                      || S <- Servers],
                     State#client{state = #commit{fix = Fix, commit_ballot = {BallotNum, Self}, waiting_acks = sets:from_list(Servers)}}};
                false ->
                    {[], State#client{state = PPState#propose{votes = Votes1}}}
            end;
        false ->
            {[], State}
    end;
%% handle commit
handle(#msg_ack
       { from = From
       , ballot = CommitBallot
       },
       #client
       { self = Self
       , servers = Servers
       , ballot_num = BallotNum
       , state = #commit{fix = Fix, commit_ballot = CommitBallot, waiting_acks = WaitingAcks} = CState
       , pending_from = {PFrom, PRef}
       } = State) ->
    case sets:is_element(From, WaitingAcks) of
        true ->
            WaitingAcks1 = sets:del_element(From, WaitingAcks),
            case sets:size(WaitingAcks1) =:= 0 of
                true when Fix =:= true ->
                    {[], State#client{state = idle, ballot_num = BallotNum + 1}, start_prepare};
                true ->
                    {[?send(PFrom, #submit_resp{ref = PRef})], State#client{state = idle, ballot_num = BallotNum + 1, pending_command = undefined, pending_from = undefined}};
                false ->
                    {[], State#client{state = CState#commit{waiting_acks = WaitingAcks1}}}
            end;
        false ->
            {[], State}
    end;
handle(_Input, State) ->
    io:format(user, "!!! Ignored input ~w~n  at state ~p~n", [_Input, State]),
    {[], State}.
