-module(prot_a_priv_bits).

-export([parse/1]).
-export([encode/1]).

-define(FLAGS, [
    wheel,
    admin,
    statistic,
    create_pers,
    create_conf,
    change_name,
    flg7,
    flg8,
    flg9,
    flg10,
    flg11,
    flg12,
    flg13,
    flg14,
    flg15,
    flg16
]).

-type t() :: #{
    wheel => boolean(),
    admin => boolean(),
    statistic => boolean(),
    create_pers => boolean(),
    create_conf => boolean(),
    change_name => boolean(),
    flg7 => boolean(),
    flg8 => boolean(),
    flg9 => boolean(),
    flg10 => boolean(),
    flg11 => boolean(),
    flg12 => boolean(),
    flg13 => boolean(),
    flg14 => boolean(),
    flg15 => boolean(),
    flg16 => boolean()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {Flags, Rest} = prot_a_bitstring:parse(List, ?FLAGS),
    {maps:from_list(Flags), Rest}.

-spec encode(t()) -> iodata().
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
