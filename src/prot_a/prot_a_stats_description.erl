-module(prot_a_stats_description).

-export([parse/1]).

-type t() :: [[prot_a_string:t() | integer()]].
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    prot_a_args:get([[prot_a_string], [prot_a_integer]], List).
