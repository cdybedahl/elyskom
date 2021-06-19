-module(prot_a_string).

-export([encode/1]).
-export([parse/1]).

parse([Str | Tail]) ->
    U = unicode:characters_to_binary(Str, latin1, utf8),
    {U, Tail}.

encode(List) when is_list(List) ->
    encode(iolist_to_binary(List));
encode(String) when is_binary(String) ->
    case unicode:characters_to_binary(String, utf8, latin1) of
        Utf8 when is_binary(Utf8) ->
            Len = integer_to_binary(byte_size(Utf8)),
            <<Len/binary, "H", Utf8/binary>>;
        {error, _, _} ->
            throw(io_lib:format("Could not convert to Latin-1: ~tp", [String]))
    end.
