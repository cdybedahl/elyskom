-module(prot_a_aux_item).

%% -export([encode/1]).
-export([parse/1]).

parse(List) ->
    {[AuxNo, Tag, Creator, CreatedAt, RawFlags, InheritLimit, Data], Tail} =
        prot_a_args:get(
            [
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_time,
                prot_a_bitstring,
                prot_a_integer,
                prot_a_string
            ],
            List
        ),
    Flags = prot_a_bitstring:annotate(
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
    {
        #{
            aux_no => AuxNo,
            tag => Tag,
            creator => Creator,
            created_at => CreatedAt,
            flags => Flags,
            inherit_limit => InheritLimit,
            data => Data
        },
        Tail
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
