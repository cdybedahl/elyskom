-module(prot_a_mark).

-include("elyskom.hrl").

-export([parse/1]).
-export([encode/1]).

encode(P) -> prot_a_pair:encode(P).
parse(List) -> prot_a_pair:parse(List).
