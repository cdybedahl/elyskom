-module(prot_a_mark).

-export([parse/1]).
-export([encode/1]).

-type t() :: prot_a_pair:t().
-export_type([t/0]).

encode(P) -> prot_a_pair:encode(P).
parse(List) -> prot_a_pair:parse(List).
