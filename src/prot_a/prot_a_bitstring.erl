-module(prot_a_bitstring).

-export([encode/1, encode/2]).
-export([parse/1, parse/2]).
-export([annotate/2]).

-type flagged_list() :: [{atom(), boolean()}].
-type bitlist() :: [boolean()].

-export_type([flagged_list/0, bitlist/0]).

-spec encode(bitlist()) -> iodata().
encode(List) when is_list(List) ->
    erlang:iolist_to_binary(
        lists:map(
            fun(I) ->
                case I of
                    true -> <<"1">>;
                    false -> <<"0">>
                end
            end,
            List
        )
    ).

-spec encode(map(), [atom()]) -> iodata().
encode(Map, Flags) ->
    BitList = lists:map(
        fun(A) ->
            maps:get(A, Map, false)
        end,
        Flags
    ),
    encode(BitList).

-spec parse([binary()]) -> {bitlist(), [binary()]}.
parse([Str | Tail]) ->
    L0 = erlang:binary_to_list(Str),
    L1 = lists:map(fun(I) -> I =:= $1 end, L0),
    {L1, Tail}.

-spec parse([binary()], [atom()]) -> {flagged_list(), [binary()]}.
parse(List, Flags) ->
    {RawFlags, Tail} = parse(List),
    {annotate(Flags, RawFlags), Tail}.

-spec annotate([atom()], [boolean()]) -> flagged_list().
annotate(Names, Bitstring) ->
    {Use, _Drop} = lists:split(length(Bitstring), Names),
    lists:zip(Use, Bitstring).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Res = encode([true, false, false, true, false]),
    ?assertEqual(<<"10010">>, Res).

parse_test() ->
    Res = parse([<<"10010">>, <<"Rest">>]),
    ?assertEqual({[true, false, false, true, false], [<<"Rest">>]}, Res).

-endif.
