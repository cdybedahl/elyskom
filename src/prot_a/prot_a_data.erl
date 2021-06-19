-module(prot_a_data).

-export([encode/1]).
-export([parse/1]).

-spec parse([binary()]) -> {binary(), [binary()]}.
parse([Str | Tail]) ->
    {Str, Tail}.

-spec encode(iodata()) -> binary().
encode(List) when is_list(List) ->
    encode(iolist_to_binary(List));
encode(String) when is_binary(String) ->
    Len = integer_to_binary(byte_size(String)),
    <<Len/binary, "H", String/binary>>.
