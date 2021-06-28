-module(prot_a_string).

-export([encode/1]).
-export([parse/1]).

-type t() :: unicode:unicode_binary().
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse([Str | Tail]) ->
    U = unicode:characters_to_binary(Str, latin1, utf8),
    {U, Tail}.

-spec encode(iodata()) -> binary() | none().
encode(List) when is_list(List) ->
    encode(iolist_to_binary(List));
encode(String) when is_binary(String) ->
    Payload =
        case unicode:characters_to_binary(String, utf8, latin1) of
            Converted when is_binary(Converted) ->
                Converted;
            {error, _, _} ->
                String
        end,
    Len = integer_to_binary(byte_size(Payload)),
    <<Len/binary, "H", Payload/binary>>.
