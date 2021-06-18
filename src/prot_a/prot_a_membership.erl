-module(prot_a_membership).

-export([encode/1]).

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

encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Res = encode(#{secret => true}),
    ?assertEqual(<<"00100000">>, Res).

-endif.
