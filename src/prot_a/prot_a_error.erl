-module(prot_a_error).

-export([parse/2]).

parse(List, _Pending) ->
    logger:error("Unknown error: ~p", [List]),
    error_wtf_is_that.