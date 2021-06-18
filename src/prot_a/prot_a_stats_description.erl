-module(prot_a_stats_description).

-export([parse/1]).

parse(List) ->
    prot_a_args:get([[prot_a_string], [prot_a_integer]], List).
