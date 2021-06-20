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

-type conf() :: #{
    rd_prot => boolean(),
    original => boolean(),
    secret => boolean(),
    letterbox => boolean()
}.

-type extended_conf() :: #{
    rd_prot => boolean(),
    original => boolean(),
    secret => boolean(),
    letterbox => boolean(),
    allow_anonymous => boolean(),
    forbid_secret => boolean(),
    reserved2 => boolean(),
    reserved3 => boolean()
}.

-type any_conf() :: conf() | extended_conf().

-export_type([conf/0, extended_conf/0, any_conf/0]).

-spec parse([binary()]) -> {any_conf(), [binary()]}.
parse(List) ->
    {Flags, Rest} = prot_a_bitstring:parse(List, ?FLAGS),
    {maps:from_list(Flags), Rest}.

-spec encode(any_conf()) -> iodata().
encode(Map) -> prot_a_bitstring:encode(Map, ?FLAGS).
