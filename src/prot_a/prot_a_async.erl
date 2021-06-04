-module(prot_a_async).

-export([parse/1]).

parse(List) ->
    do_parse(List).

do_parse([<<"7">>]) ->
    async_saving_database;
do_parse([<<"9">> | Tail]) ->
    [PersNo, SessionNo] = prot_a_args:parse([prot_a_integer, prot_a_integer], Tail),
    {async_login, PersNo, SessionNo};
do_parse([<<"13">> | Tail]) ->
    [PersNo, SessionNo] = prot_a_args:parse([prot_a_integer, prot_a_integer], Tail),
    {async_logout, PersNo, SessionNo};
do_parse(List) ->
    {async_wtf_is_that, List}.