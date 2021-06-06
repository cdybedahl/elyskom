-module(elyskom_call).

-include("elyskom.hrl").

-export([make/2]).

make(login, Args) ->
    process([prot_a_integer, prot_a_integer, prot_a_string, prot_a_bool], [62 | Args]);
make(accept_async, AsyncList) ->
    process([prot_a_integer, [prot_a_integer]], [80 | AsyncList]).

process(Pattern, Pairs) ->
    Elements = lists:map(
        fun({[Type], Data}) ->
            prot_a_array:encode(Type, Data);
           ({Type, Data}) when is_atom(Type) ->
            Type:encode(Data)
        end,
        lists:zip(Pattern, Pairs)
    ),
    lists:join(<<" ">>, Elements).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accept_async_test() ->
    Res = ?l2b(make(accept_async, lists:seq(2,10,2))),
    ?assertEqual(<<"80 5 { 2 4 6 8 10 }">>, Res).

-endif.