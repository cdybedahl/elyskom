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

parse(List) -> prot_a_bitstring:parse(List, ?FLAGS).
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
