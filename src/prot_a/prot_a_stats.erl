-module(prot_a_stats).

-include("elyskom.hrl").

-export([parse/1]).

parse([Average, AscentRate, DescentRate | Tail]) ->
    {
        [
            ?b2f(Average),
            ?b2f(AscentRate),
            ?b2f(DescentRate)
        ],
        Tail
    }.
