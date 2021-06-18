-module(prot_a_extended_conf).

-export([parse/1]).
-export([encode/1]).

-define(FLAGS, [
    rd_prot,
    original,
    secret,
    letterbox,
    allow_anonymous,
    forbid_secret,
    reserved2,
    reserved3
]).

parse(List) -> prot_a_bitstring:parse(List, ?FLAGS).
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
