-module(prot_a_membership_type).

-export([encode/1]).
-export([parse/1]).

-define(FLAGS, [
    invitation,
    passive,
    secret,
    passive_message_invert,
    reserved2,
    reserved3,
    reserved4,
    reserved5
]).

-type t() :: #{
    invitation => boolean(),
    passive => boolean(),
    secret => boolean(),
    passive_message_invert => boolean(),
    reserved2 => boolean(),
    reserved3 => boolean(),
    reserved4 => boolean(),
    reserved5 => boolean()
}.
-export_type([t/0]).

-spec encode(t()) -> iodata().
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) -> prot_a_bitstring:parse(List, ?FLAGS).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Res = encode(#{secret => true}),
    ?assertEqual(<<"00100000">>, Res).

-endif.
