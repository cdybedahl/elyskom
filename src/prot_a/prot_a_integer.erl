-module(prot_a_integer).

-export([encode/1]).
-export([parse/1]).

encode(Int) when is_integer(Int) ->
    erlang:integer_to_binary(Int).

parse([RawInt | Tail]) ->
    {binary_to_integer(RawInt), Tail}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Res = parse([<<"17">>, <<"Rest">>]),
    ?assertEqual({17, [<<"Rest">>]}, Res).

encode_test() ->
    Res = encode(17),
    ?assertEqual(<<"17">>, Res).

-endif.