-module(prot_a_personal_flags).

-export([parse/1]).
-export([encode/1]).

-define(FLAGS, [
    unread_is_secret,
    flg2,
    flg3,
    flg4,
    flg5,
    flg6,
    flg7,
    flg8
]).

-type t() :: #{
    unread_is_secret => boolean(),
    flg2 => boolean(),
    flg3 => boolean(),
    flg4 => boolean(),
    flg5 => boolean(),
    flg6 => boolean(),
    flg7 => boolean(),
    flg8 => boolean()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {Flags, Rest} = prot_a_bitstring:parse(List, ?FLAGS),
    {maps:from_list(Flags), Rest}.

-spec encode(t()) -> iodata().
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
