-module(prot_a_bool).

-export([encode/1]).
-export([parse/1]).

encode(true) ->
    <<"1">>;
encode(false) ->
    <<"0">>.

parse([<<"1">> | Tail]) ->
    {true, Tail};
parse([<<"0">> | Tail]) ->
    {false, Tail}.