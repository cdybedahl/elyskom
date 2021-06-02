-module(prot_a_bool).

-export([encode/1]).

encode(true) ->
    <<"1">>;
encode(false) ->
    <<"0">>.