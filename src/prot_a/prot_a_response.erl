-module(prot_a_response).

-export([parse/2]).

parse(List, _Pending) ->
    logger:error("Unknown response: ~p", [List]),
    response_wtf_is_that.