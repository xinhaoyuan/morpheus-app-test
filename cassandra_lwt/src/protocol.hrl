-type client_id()  :: term().
-type server_id()  :: term().
-type ballot_num() :: non_neg_integer().
-type ballot_id()  :: {ballot_num(), client_id()}.
-type item()       :: term().

-record(submit_req, {from :: pid(), ref :: reference(), command :: item()}).
-record(submit_resp, {ref :: reference()}).
-record(get_commit_history_req, {from :: pid(), ref :: reference()}).
-record(get_commit_history_resp, {ref :: reference(), history :: [item()]}).

-define(send(To, Content), {send, To, Content}).

-record(msg_prepare,
        { from             :: client_id()
        , ballot           :: ballot_id()
        }).

-record(msg_prepare_reject,
        { from             :: server_id()
        , ballot           :: ballot_id()
        , rejected_ballot  :: ballot_id()
        }).

-record(msg_promise,
        { from             :: server_id()
        , ballot           :: ballot_id()
        , accepted         :: undefined | {ballot_id(), item()}
        , committed        :: undefined | {ballot_id(), item()}
        }).

-record(msg_propose,
        { from             :: client_id()
        , ballot           :: ballot_id()
        , item             :: item()
        }).

-record(msg_accepted,
        { from             :: server_id()
        , ballot           :: ballot_id()
        }).

-record(msg_commit,
        { from             :: client_id()
        , ballot           :: ballot_id()
        , item             :: item()
        }).

-record(msg_ack,
        { from             :: server_id()
        , ballot           :: ballot_id()
        }).

% -define(verbose(F, A), io:format(user, F, A)).
-define(verbose(F, A), ok).
