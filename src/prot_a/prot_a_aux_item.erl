-module(prot_a_aux_item).

%% -export([encode/1]).
-export([parse/1]).

parse(List) ->
    {[AuxNo, Tag, Creator, CreatedAt, Flags, InheritLimit, Data], Tail} =
        prot_a_args:get(
            [
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_time,
                prot_a_aux_item_flags,
                prot_a_integer,
                prot_a_string
            ],
            List
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
