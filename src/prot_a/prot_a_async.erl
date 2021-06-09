-module(prot_a_async).

-include("elyskom.hrl").

-export([parse/1]).

parse(List) ->
    do_parse(List).

do_parse([<<"0">> | Tail]) ->
    [TextNo, TextStatOld] = prot_a_args:parse([prot_a_integer, prot_a_textstat_old], Tail),
    {async_new_text_old, TextNo, TextStatOld};
do_parse([<<"6">> | Tail]) ->
    [PersNo, ConfNo, SessionNo, What, UName] =
        prot_a_args:parse(
            [prot_a_integer, prot_a_integer, prot_a_integer, prot_a_string, prot_a_string],
            Tail
        ),
    {async_i_am_on, PersNo, ConfNo, SessionNo, What, UName};
do_parse([<<"7">>]) ->
    async_saving_database;
do_parse([<<"9">> | Tail]) ->
    [PersNo, SessionNo] = prot_a_args:parse([prot_a_integer, prot_a_integer], Tail),
    {async_login, PersNo, SessionNo};
do_parse([<<"13">> | Tail]) ->
    [PersNo, SessionNo] = prot_a_args:parse([prot_a_integer, prot_a_integer], Tail),
    {async_logout, PersNo, SessionNo};
do_parse([<<"15">> | Tail]) ->
    [TextNo, TextStat] = prot_a_args:parse([prot_a_integer, prot_a_textstat], Tail),
    {async_new_text, TextNo, TextStat};
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
    ),
    Incoming15 = [
        <<"15">>,
        <<"24750475">>,
        <<"41">>,
        <<"4">>,
        <<"19">>,
        <<"9">>,
        <<"5">>,
        <<"121">>,
        <<"3">>,
        <<"159">>,
        <<"1">>,
        <<"1370">>,
        <<"10">>,
        <<"572">>,
        <<"0">>,
        <<"3">>,
        <<"{">>,
        <<"2">>,
        <<"24749576">>,
        <<"0">>,
        <<"6">>,
        <<"6">>,
        <<"4164968">>,
        <<"}">>,
        <<"2">>,
        <<"{">>,
        <<"1">>,
        <<"15">>,
        <<"1370">>,
        <<"41">>,
        <<"4">>,
        <<"19">>,
        <<"9">>,
        <<"5">>,
        <<"121">>,
        <<"3">>,
        <<"159">>,
        <<"1">>,
        <<"00000000">>,
        <<"0">>,
        <<"lyskom.el 0.48 (2016-08-28; Git 372be66)">>,
        <<"2">>,
        <<"1">>,
        <<"1370">>,
        <<"41">>,
        <<"4">>,
        <<"19">>,
        <<"9">>,
        <<"5">>,
        <<"121">>,
        <<"3">>,
        <<"159">>,
        <<"1">>,
        <<"00000000">>,
        <<"1">>,
        <<"text/x-kom-basic;charset=iso-8859-1">>,
        <<"}">>
    ],
    Res2 = parse(Incoming15),
    ?assertEqual(
        {async_new_text, 24750475, #{
            author => 1370,
            aux_item =>
                [
                    #{
                        aux_no => 1,
                        created_at => {{2021, 6, 9}, {19, 4, 41}},
                        creator => 1370,
                        data =>
                            <<"lyskom.el 0.48 (2016-08-28; Git 372be66)">>,
                        flags =>
                            [
                                {deleted, false},
                                {inherit, false},
                                {secret, false},
                                {hide_creator, false},
                                {dont_garb, false},
                                {reserved2, false},
                                {reserved3, false},
                                {reserved4, false}
                            ],
                        inherit_limit => 0,
                        tag => 15
                    },
                    #{
                        aux_no => 2,
                        created_at => {{2021, 6, 9}, {19, 4, 41}},
                        creator => 1370,
                        data =>
                            <<"text/x-kom-basic;charset=iso-8859-1">>,
                        flags =>
                            [
                                {deleted, false},
                                {inherit, false},
                                {secret, false},
                                {hide_creator, false},
                                {dont_garb, false},
                                {reserved2, false},
                                {reserved3, false},
                                {reserved4, false}
                            ],
                        inherit_limit => 1,
                        tag => 1
                    }
                ],
            creation_time => {{2021, 6, 9}, {19, 4, 41}},
            misc_info =>
                [
                    {comm_to, 24749576},
                    {recpt, 6},
                    {loc_no, 4164968}
                ],
            no_of_chars => 572,
            no_of_lines => 10,
            no_of_marks => 0
        }},
        Res2
    ).

-endif.
