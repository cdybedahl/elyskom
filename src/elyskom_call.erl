-module(elyskom_call).

-export([make/2]).

make(login, Args) ->
    Pairs = lists:zip([prot_a_integer, prot_a_integer, prot_a_string, prot_a_bool], [62 | Args]),
    Parts = process(Pairs),
    Parts.

process(List) ->
    Elements = lists:map(fun({Type, Data}) -> Type:encode(Data) end, List),
    lists:join(<<" ">>, Elements).