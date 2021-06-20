-module(prot_a_aux_item).

%% -export([encode/1]).
-export([parse/1]).
-export([encode/1]).

-type t() :: #{
    aux_no => pos_integer(),
    tag => pos_integer(),
    creator => pos_integer(),
    created_at => prot_a_time:t(),
    flags => prot_a_aux_item_flags:t(),
    inherit_limit => pos_integer(),
    data => prot_a_string:t()
}.
-type input() :: #{
    tag => pos_integer(),
    flags => prot_a_aux_item_flags:t(),
    inherit_limit => pos_integer(),
    data => prot_a_string:t()
}.
-export_type([t/0, input/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
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

-spec encode(input()) -> iodata().
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
