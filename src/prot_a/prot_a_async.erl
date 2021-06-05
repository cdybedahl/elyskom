-module(prot_a_async).

-include("elyskom.hrl").

-export([parse/1]).

parse(List) ->
    do_parse(List).

do_parse([<<"0">> | Tail]) ->
    [TextNo, TextStatOld] = prot_a_args:parse([prot_a_integer, prot_a_textstat_old], Tail),
    {async_new_text_old, TextNo, TextStatOld};
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
parse_test() ->
    Incoming = [
        <<"0">>,
        <<"24741877">>,
        <<"45">>,
        <<"26">>,
        <<"9">>,
        <<"5">>,
        <<"5">>,
        <<"121">>,
        <<"6">>,
        <<"155">>,
        <<"1">>,
        <<"2704">>,
        <<"1">>,
        <<"57">>,
        <<"0">>,
        <<"3">>,
        <<"{">>,
        <<"2">>,
        <<"24740749">>,
        <<"0">>,
        <<"6">>,
        <<"6">>,
        <<"4163770">>,
        <<"}">>
    ],
    Res = parse(Incoming),
    ?assertEqual(
        {async_new_text_old, 24741877, #{
            author => 2704,
            creation_time => {{2021, 6, 5}, {9, 26, 45}},
            misc_info =>
                [{comm_to, 24740749}, {recpt, 6}, {loc_no, 4163770}],
            no_of_chars => 57,
            no_of_lines => 1,
            no_of_marks => 0
        }},
        Res
    ).

-endif.
