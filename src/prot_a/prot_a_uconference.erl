-module(prot_a_uconference).

-export([parse/1]).

-type t() :: #{
    name => binary(),
    highest_local_no => pos_integer(),
    garb_nice => pos_integer(),
    type => prot_a_bitstring:bitlist()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {[A, B, C, D], R} =
        prot_a_args:get([prot_a_string, prot_a_bitstring, prot_a_integer, prot_a_integer], List),
    {
        #{
            name => A,
            type => B,
            highest_local_no => C,
            garb_nice => D
        },
        R
    }.
