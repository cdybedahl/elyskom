-module(prot_a_array).

-export([parse/2]).

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

-endif.
