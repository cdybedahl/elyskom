-module(prot_a_read_range).

-include("elyskom.hrl").

-export([parse/1]).
-export([encode/1]).

encode({First, Last}) ->
    F = ?i2b(First),
    L = ?i2b(Last),
    <<F/binary, " ", L/binary>>.

parse([{First, Last} | Tail]) ->
    {
        {?b2i(First), ?b2i(Last)},
        Tail
    }.
