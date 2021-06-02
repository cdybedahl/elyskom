-module(elyskom_call).

-export([make/2]).

make(login, Args) ->
    process([prot_a_integer, prot_a_integer, prot_a_string, prot_a_bool], [62 | Args]).

process(Pattern, Pairs) ->
    Elements = lists:map(
        fun({Type, Data}) ->
            Type:encode(Data)
        end,
        lists:zip(Pattern, Pairs)
    ),
    lists:join(<<" ">>, Elements).
