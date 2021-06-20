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

-type t() :: #{
    deleted => boolean(),
    inherit => boolean(),
    secret => boolean(),
    hide_creator => boolean(),
    dont_garb => boolean()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {prot_a_bitstring:flagged_list(), [binary()]}.
parse(List) -> prot_a_bitstring:parse(List, ?FLAGS).

-spec encode(t()) -> iodata().
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
