-module(prot_a_response).

-include("elyskom.hrl").

-export([parse/3]).

parse(Type, [RefNo | Tail], Pending) ->
    [{RefNo, CallName, From}] = ets:lookup(Pending, RefNo),
    ets:delete(Pending, RefNo),
    Res = do_parse(Type, CallName, Tail),
    gen_statem:reply(From, Res);
parse(Type, List, _Pending) ->
    logger:error("Unknown ~p: ~p", [Type, List]),
    response_wtf_is_that.

do_parse(response, get_time, Args) ->
    [Time] = prot_a_args:parse([prot_a_time], Args),
    Time;
do_parse(response, get_text, Args) ->
    [Text] = prot_a_args:parse([prot_a_string], Args),
    Text;
do_parse(response, get_uconf_stat, Args) ->
    [Stat] = prot_a_args:parse([prot_a_uconference], Args),
    Stat;
do_parse(response, _CallName, []) ->
    ok;
do_parse(error, _CallName, Args) ->
    [Code, Status] = prot_a_args:parse([prot_a_integer, prot_a_integer], Args),
    {error, prot_a_error:to_atom(Code), Status}.
