-module(prot_a_member).

-export([parse/1]).

-type t() :: #{
    member => pos_integer(),
    added_by => pos_integer(),
    added_at => prot_a_time:t(),
    type => prot_a_membership_type:t()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {[Member, AddedBy, AddedAt, Type], Rest} = prot_a_args:get(
        [prot_a_integer, prot_a_integer, prot_a_time, prot_a_membership_type],
        List
    ),
    {
        #{
            member => Member,
            added_by => AddedBy,
            added_at => AddedAt,
            type => Type
        },
        Rest
    }.
