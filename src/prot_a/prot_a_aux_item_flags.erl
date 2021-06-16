-module(prot_a_aux_item_flags).

-export([parse/1]).
-export([encode/1]).

parse(List) ->
    {RawFlags, Tail} = prot_a_bitstring:parse(List),
    {
        prot_a_bitstring:annotate(
            [
                deleted,
                inherit,
                secret,
                hide_creator,
                dont_garb,
                reserved2,
                reserved3,
                reserved4
            ],
            RawFlags
        ),
        Tail
    }.

encode(Map) ->
    BitList = lists:map(
        fun(A) ->
            maps:get(A, Map, false)
        end,
        [
            deleted,
            inherit,
            secret,
            hide_creator,
            dont_garb,
            reserved2,
            reserved3,
            reserved4
        ]
    ),
    prot_a_bitstring:encode(BitList).
