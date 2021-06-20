-module(prot_a_bool).

-export([encode/1]).
-export([parse/1]).

-spec encode(boolean()) -> binary().
encode(true) ->
    <<"1">>;
encode(false) ->
    <<"0">>.

-spec parse([binary()]) -> {boolean(), [binary()]}.
parse([<<"1">> | Tail]) ->
    {true, Tail};
parse([<<"0">> | Tail]) ->
    {false, Tail}.
