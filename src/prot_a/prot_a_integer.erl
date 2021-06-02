-module(prot_a_integer).

-export([encode/1]).

encode(Int) when is_integer(Int) ->
    erlang:integer_to_binary(Int).