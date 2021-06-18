-module(prot_a_membership).

-export([parse/1]).

parse(List) ->
    {[Position, LastTimeRead, Conference, Priority, ReadRanges, AddedBy, AddedAt, Type], Rest} = prot_a_args:parse(
        [
            prot_a_integer,
            prot_a_time,
            prot_a_integer,
            prot_a_integer,
            [prot_a_read_range],
            prot_a_integer,
            prot_a_time,
            prot_a_membership_type
        ],
        List
    ),
    {
        #{
            position => Position,
            last_time_read => LastTimeRead,
            conference => Conference,
            priority => Priority,
            read_ranges => ReadRanges,
            added_by => AddedBy,
            added_at => AddedAt,
            type => Type
        },
        Rest
    }.
