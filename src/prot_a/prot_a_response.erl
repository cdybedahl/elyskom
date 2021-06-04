-module(prot_a_response).

-export([parse/2]).

parse([RefNo | Tail], Pending) ->
    [{RefNo, CallName, From}] = ets:lookup(Pending, RefNo),
    ets:delete(Pending, RefNo),
    do_parse(CallName, Tail, From);
parse(List, _Pending) ->
    logger:error("Unknown response: ~p", [List]),
    response_wtf_is_that.

do_parse(login, [], From) ->
    gen_statem:reply(From, ok).