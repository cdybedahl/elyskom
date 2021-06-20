-module(prot_a_pair).

-include("elyskom.hrl").

-export([parse/1]).
-export([encode/1]).

-type t() :: {integer(), integer()}.
-export_type([t/0]).

-spec encode(t()) -> iodata().
encode({First, Last}) ->
    F = ?i2b(First),
    L = ?i2b(Last),
    <<F/binary, " ", L/binary>>.

-spec parse([binary()]) -> {t(), [binary()]}.
parse([First, Last | Tail]) ->
    {
        {?b2i(First), ?b2i(Last)},
        Tail
    }.
