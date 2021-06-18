-module(prot_a_aux_item_flags).

-export([parse/1]).
-export([encode/1]).

-define(FLAGS, [
    deleted,
    inherit,
    secret,
    hide_creator,
    dont_garb,
    reserved2,
    reserved3,
    reserved4
]).

parse(List) -> prot_a_bitstring:parse(List, ?FLAGS).
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
