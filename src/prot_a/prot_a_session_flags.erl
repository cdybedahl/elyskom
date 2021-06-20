-module(prot_a_session_flags).

-export([parse/1]).
-export([encode/1]).

-define(FLAGS, [
    invisible,
    user_active_used,
    user_absent,
    reserved3,
    reserved4,
    reserved5,
    reserved6,
    reserved7
]).

-type t() :: #{
    invisible => boolean(),
    user_active_used => boolean(),
    user_absent => boolean(),
    reserved3 => boolean(),
    reserved4 => boolean(),
    reserved5 => boolean(),
    reserved6 => boolean(),
    reserved7 => boolean()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {prot_a_bitstring:flagged_list(), [binary()]}.
parse(List) -> prot_a_bitstring:parse(List, ?FLAGS).

-spec encode(t()) -> iodata().
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
