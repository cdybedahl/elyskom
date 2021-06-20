-module(prot_a_membership).

-export([parse/1]).

-type t() :: #{
    position => pos_integer(),
    last_time_read => prot_a_time:t(),
    conference => pos_integer(),
    priority => pos_integer(),
    read_ranges => [prot_a_read_range:t()],
    added_by => pos_integer(),
    added_at => prot_a_time:t(),
    type => prot_a_membership_type:t()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {[Position, LastTimeRead, Conference, Priority, ReadRanges, AddedBy, AddedAt, Type], Rest} = prot_a_args:get(
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
