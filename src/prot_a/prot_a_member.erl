-module(prot_a_member).

-export([parse/1]).

parse(List) ->
    {[Member, AddedBy, AddedAt, Type], rest} = prot_a_args:get(
        [prot_a_integer, prot_a_integer, prot_a_time, prot_a_membership_type],
        List
    ),
    {
        #{
            member => Member,
            added_by => AddedBy,
            added_at => AddedAt,
            type => Type
        }
    }.
