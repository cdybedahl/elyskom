-module(elyskom_call).

-export([make/2]).
-export([response/2]).

make(CallName, Args) ->
    Pattern = make_args(CallName),
    Pairs = [call_no(CallName) | Args],
    Elements = lists:map(
        fun
            ({[Type], Data}) ->
                prot_a_array:encode(Type, Data);
            ({Type, Data}) when is_atom(Type) ->
                Type:encode(Data)
        end,
        lists:zip([prot_a_integer | Pattern], Pairs)
    ),
    lists:join(<<" ">>, Elements).

call_no(get_time) -> 35;
call_no(get_text) -> 25;
call_no(get_uconf_stat) -> 78;
call_no(login) -> 62;
call_no(accept_async) -> 80.

make_args(get_time) -> [];
make_args(get_text) -> [prot_a_integer, prot_a_integer, prot_a_integer];
make_args(get_uconf_stat) -> [prot_a_integer];
make_args(login) -> [prot_a_integer, prot_a_string, prot_a_bool];
make_args(accept_async) -> [[prot_a_integer]].

response(get_time, Args) ->
    [Time] = prot_a_args:parse([prot_a_time], Args),
    Time;
response(get_text, Args) ->
    [Text] = prot_a_args:parse([prot_a_string], Args),
    Text;
response(get_uconf_stat, Args) ->
    [Stat] = prot_a_args:parse([prot_a_uconference], Args),
    Stat;
response(_CallName, []) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accept_async_test() ->
    Res = ?l2b(make(accept_async, lists:seq(2, 10, 2))),
    ?assertEqual(<<"80 5 { 2 4 6 8 10 }">>, Res).

-endif.
