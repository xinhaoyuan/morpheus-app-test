-module(locks_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 120, ?_test( t1() )}.

t1() ->
    ok = application:ensure_started(locks),
    Me = self(),
    Size = 3,
    lists:foreach(fun (Id) -> spawn(fun () -> t1_c(Me, Id) end) end, lists:seq(1, Size)),
    lists:foreach(fun (Id) -> receive {Id, ok} -> io:format("~p~n", [{Id, finished}]) end end, lists:seq(1, Size)),
    ok.

t1_c(CtrlProc, Id) ->
    %% Make agents in particular order of 3. < 2. < 1.
    case Id of
        1 -> timer:sleep(500);
        2 -> timer:sleep(250);
        3 -> ok
    end,
    {ok, Agt} = locks_agent:start(),
    %% Align the time
    case Id of
        1 -> ok;
        2 -> timer:sleep(250);
        3 -> timer:sleep(500)
    end,
    case Id of
        1 ->
            locks:lock(Agt, [1], write),
            locks:lock(Agt, [2], write),
            timer:sleep(500), io:format(user, "SP 2~n", []),
            locks:lock(Agt, [3], write),
            ok;
        2 ->
            timer:sleep(750), io:format(user, "SP 3~n", []),
            locks:lock(Agt, [2], write),
            locks:lock(Agt, [3], write),
            locks:lock(Agt, [1], write),
            ok;
        3 ->
            locks:lock(Agt, [3], write),
            timer:sleep(250), io:format(user, "SP 1~n", []),
            locks:lock(Agt, [1], write),
            %% [1] is surrendered until SP3, so 750ms will be spent until this point instead of 500ms.
            %% Delaying 500 is enough to enforce the order
            timer:sleep(500), io:format(user, "SP 4~n", []),
            locks:lock(Agt, [2], write),
            ok
    end,
    locks:end_transaction(Agt),
    CtrlProc ! {Id, ok}.
