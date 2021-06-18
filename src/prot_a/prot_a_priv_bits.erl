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

parse(List) -> prot_a_bitstring:parse(List, ?FLAGS).
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
