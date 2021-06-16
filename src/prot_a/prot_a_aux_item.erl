-module(prot_a_aux_item).

%% -export([encode/1]).
-export([parse/1]).
-export([encode/1]).

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

encode(#{tag := Tag, flags := Flags, inherit_limit := InheritLimit, data := Data}) ->
    iolist_to_binary(
        lists:join(<<" ">>, [
            prot_a_integer:encode(Tag),
            prot_a_aux_item_flags:encode(Flags),
            prot_a_integer:encode(InheritLimit),
            prot_a_string:encode(Data)
        ])
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
