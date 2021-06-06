-module(prot_a_array).

-include("elyskom.hrl").

-export([parse/2]).
-export([encode/2]).

parse([RawCount, <<"{">> | Rest0], Type) ->
    Count = binary_to_integer(RawCount),
    {Args, Rest1} =
        lists:foldl(
            fun(_, {Acc, R0}) ->
                {A, R1} = Type:parse(R0),
                {[A | Acc], R1}
            end,
            {[], Rest0},
            lists:seq(1, Count)
        ),
    [<<"}">> | Rest2] = Rest1,
    {lists:reverse(Args), Rest2}.

encode(Type, List) ->
    Count = ?i2b(length(List)),
    Items = lists:map(fun Type:encode/1, List),
    lists:join(<<" ">>, [Count, <<"{">>] ++ Items ++ [<<"}">>]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Incoming = [
        <<"5">>,
        <<"{">>,
        <<"45">>,
        <<"26">>,
        <<"9">>,
        <<"5">>,
        <<"5">>,
        <<"}">>,
        <<"Extra">>
    ],
    Res = parse(Incoming, prot_a_integer),
    ?assertEqual({[45, 26, 9, 5, 5], [<<"Extra">>]}, Res).

produce_test() ->
    List = lists:seq(1,5),
    Res = ?l2b(encode(prot_a_integer, List)),
    ?assertEqual(<<"5 { 1 2 3 4 5 }">>, Res).

-endif.
