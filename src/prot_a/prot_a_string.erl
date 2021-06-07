-module(prot_a_string).

-export([encode/1]).
-export([parse/1]).

parse([Str | Tail]) ->
    {Str, Tail}.

encode(String) when is_binary(String) ->
    Len = integer_to_binary(byte_size(String)),
    <<Len/binary, "H", String/binary>>;
encode(List) when is_list(List) ->
    String = iolist_to_binary(List),
    Len = integer_to_binary(byte_size(String)),
    <<Len/binary, "H", String/binary>>.