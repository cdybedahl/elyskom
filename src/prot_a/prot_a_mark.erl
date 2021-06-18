-module(prot_a_mark).

-include("elyskom.hrl").

-export([parse/1]).
-export([encode/1]).

encode({TextNo, Type}) ->
    F = ?i2b(TextNo),
    L = ?i2b(Type),
    <<F/binary, " ", L/binary>>.

parse([TextNo, Type | Tail]) ->
    {
        {?b2i(TextNo), ?b2i(Type)},
        Tail
    }.
