-module(prot_a_stats).

-include("elyskom.hrl").

-export([parse/1]).

-type t() :: [float()].
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse([Average, AscentRate, DescentRate | Tail]) ->
    {
        [
            ?b2f(Average),
            ?b2f(AscentRate),
            ?b2f(DescentRate)
        ],
        Tail
    }.
