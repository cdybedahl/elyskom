%% @hidden
-module(elyskom_call).

-export([make/2]).
-export([response/2]).

-spec make(atom(), list()) -> iodata().
make(CallName, Args) ->
    Pattern = make_args(CallName),
    Pairs = [call_no(CallName) | Args],
    Elements = lists:map(
        fun
            ({[Type], Data}) ->
                prot_a_array:encode(Type, Data);
            ({Type, Data}) when is_atom(Type) ->
                Type:encode(Data)
        end,
        lists:zip([prot_a_integer | Pattern], Pairs)
    ),
    lists:join(<<" ">>, Elements).

%%% Protocol A call numbers
-spec call_no(atom()) -> integer().
call_no(re_z_lookup) -> 74;
call_no(lookup_z_name) -> 76;
call_no(who_is_on_dynamic) -> 83;
call_no(get_text_stat) -> 90;
call_no(get_time) -> 35;
call_no(get_text) -> 25;
call_no(get_uconf_stat) -> 78;
call_no(login) -> 62;
call_no(accept_async) -> 80;
call_no(add_comment) -> 32;
call_no(add_footnote) -> 37;
call_no(add_member) -> 100;
call_no(add_recipient) -> 30;
call_no(change_conference) -> 2;
call_no(change_name) -> 3;
call_no(change_what_i_am_doing) -> 4;
call_no(create_anonymous_text) -> 87;
call_no(create_conf) -> 88;
call_no(create_person) -> 89;
call_no(create_text) -> 86;
call_no(delete_conf) -> 11;
call_no(delete_text) -> 29;
call_no(disconnect) -> 55;
call_no(enable) -> 42;
call_no(find_next_conf_no) -> 116;
call_no(find_next_text_no) -> 60;
call_no(find_previous_conf_no) -> 117;
call_no(find_previous_text_no) -> 61;
call_no(first_unused_conf_no) -> 114;
call_no(first_unused_text_no) -> 115;
call_no(get_boottime_info) -> 113;
call_no(get_client_name) -> 70;
call_no(get_client_version) -> 71;
call_no(get_collate_table) -> 85;
call_no(get_conf_stat) -> 91;
call_no(get_info) -> 94;
call_no(get_last_text) -> 58;
call_no(get_marks) -> 23;
call_no(get_members) -> 101;
call_no(get_membership) -> 108;
call_no(get_person_stat) -> 49;
call_no(get_static_session_info) -> 84;
call_no(get_stats) -> 112;
call_no(get_stats_description) -> 111;
call_no(get_unread_confs) -> 52;
call_no(get_version_info) -> 75;
call_no(local_to_global) -> 103;
call_no(local_to_global_reverse) -> 121;
call_no(logout) -> 1;
call_no(map_created_texts) -> 104;
call_no(map_created_texts_reverse) -> 122;
call_no(mark_as_read) -> 27;
call_no(mark_as_unread) -> 109;
call_no(mark_text) -> 72;
call_no(modify_conf_info) -> 93;
call_no(modify_system_info) -> 95;
call_no(modify_text_info) -> 92;
call_no(query_async) -> 81;
call_no(query_predefined_aux_items) -> 96;
call_no(query_read_texts) -> 107;
call_no(send_message) -> 53;
call_no(set_client_version) -> 69;
call_no(set_conf_type) -> 21;
call_no(set_connection_time_format) -> 120;
call_no(set_etc_motd) -> 17;
call_no(set_garb_nice) -> 22;
call_no(set_info) -> 79;
call_no(set_keep_commented) -> 105;
call_no(set_last_read) -> 77;
call_no(set_membership_type) -> 102;
call_no(set_motd_of_lyskom) -> 41;
call_no(set_passwd) -> 8;
call_no(set_permitted_submitters) -> 19;
call_no(set_pers_flags) -> 106;
call_no(set_presentation) -> 16;
call_no(set_priv_bits) -> 7;
call_no(set_read_ranges) -> 110;
call_no(set_super_conf) -> 20;
call_no(set_supervisor) -> 18;
call_no(set_unread) -> 40;
call_no(set_user_area) -> 57;
call_no(shutdown_kom) -> 44;
call_no(sub_comment) -> 33;
call_no(sub_footnote) -> 38;
call_no(sub_member) -> 15;
call_no(sub_recipient) -> 31;
call_no(sync_kom) -> 43;
call_no(unmark_text) -> 73;
call_no(user_active) -> 82;
call_no(who_am_i) -> 56.

%%% Lists of argument types for calls
-spec make_args(atom()) -> [atom()].
make_args(re_z_lookup) ->
    [prot_a_string, prot_a_bool, prot_a_bool];
make_args(lookup_z_name) ->
    [prot_a_string, prot_a_bool, prot_a_bool];
make_args(who_is_on_dynamic) ->
    [prot_a_bool, prot_a_bool, prot_a_integer];
make_args(get_text_stat) ->
    [prot_a_integer];
make_args(get_time) ->
    [];
make_args(get_text) ->
    [prot_a_integer, prot_a_integer, prot_a_integer];
make_args(get_uconf_stat) ->
    [prot_a_integer];
make_args(login) ->
    [prot_a_integer, prot_a_string, prot_a_bool];
make_args(accept_async) ->
    [[prot_a_integer]];
make_args(add_comment) ->
    [prot_a_integer, prot_a_integer];
make_args(add_footnote) ->
    [prot_a_integer, prot_a_integer];
make_args(add_member) ->
    [prot_a_integer, prot_a_integer, prot_a_integer, prot_a_integer, prot_a_membership_type];
make_args(add_recipient) ->
    [prot_a_integer, prot_a_integer, prot_a_info];
make_args(change_conference) ->
    [prot_a_integer];
make_args(change_name) ->
    [prot_a_integer, prot_a_string];
make_args(change_what_i_am_doing) ->
    [prot_a_string];
make_args(create_anonymous_text) ->
    [prot_a_string, [prot_a_misc_info], [prot_a_aux_item]];
make_args(create_conf) ->
    [prot_a_string, prot_a_extended_conf, [prot_a_aux_item]];
make_args(create_person) ->
    [prot_a_string, prot_a_string, prot_a_personal_flags, [prot_a_aux_item]];
make_args(create_text) ->
    [prot_a_string, [prot_a_misc_info], [prot_a_aux_item]];
make_args(delete_conf) ->
    [prot_a_integer];
make_args(delete_text) ->
    [prot_a_integer];
make_args(disconnect) ->
    [prot_a_integer];
make_args(enable) ->
    [prot_a_integer];
make_args(find_next_conf_no) ->
    [prot_a_integer];
make_args(find_next_text_no) ->
    [prot_a_integer];
make_args(find_previous_conf_no) ->
    [prot_a_integer];
make_args(find_previous_text_no) ->
    [prot_a_integer];
make_args(first_unused_conf_no) ->
    [];
make_args(first_unused_text_no) ->
    [];
make_args(get_boottime_info) ->
    [];
make_args(get_client_name) ->
    [prot_a_integer];
make_args(get_client_version) ->
    [prot_a_integer];
make_args(get_collate_table) ->
    [];
make_args(get_conf_stat) ->
    [prot_a_integer];
make_args(get_info) ->
    [];
make_args(get_last_text) ->
    [prot_a_time];
make_args(get_marks) ->
    [];
make_args(get_members) ->
    [prot_a_integer, prot_a_integer, prot_a_integer];
make_args(get_membership) ->
    [prot_a_integer, prot_a_integer, prot_a_integer, prot_a_bool, prot_a_integer];
make_args(get_person_stat) ->
    [prot_a_integer];
make_args(get_static_session_info) ->
    [prot_a_integer];
make_args(get_stats) ->
    [prot_a_string];
make_args(get_stats_description) ->
    [];
make_args(get_unread_confs) ->
    [prot_a_integer];
make_args(get_version_info) ->
    [];
make_args(local_to_global) ->
    [prot_a_integer, prot_a_integer, prot_a_integer];
make_args(local_to_global_reverse) ->
    [prot_a_integer, prot_a_integer, prot_a_integer];
make_args(logout) ->
    [];
make_args(map_created_texts) ->
    [prot_a_integer, prot_a_integer, prot_a_integer];
make_args(map_created_texts_reverse) ->
    [prot_a_integer, prot_a_integer, prot_a_integer];
make_args(mark_as_read) ->
    [prot_a_integer, [prot_a_integer]];
make_args(mark_as_unread) ->
    [prot_a_integer, prot_a_integer];
make_args(mark_text) ->
    [prot_a_integer, prot_a_integer];
make_args(modify_conf_info) ->
    [prot_a_integer, [prot_a_integer], [prot_a_aux_item]];
make_args(modify_system_info) ->
    [[prot_a_integer], [prot_a_aux_item]];
make_args(modify_text_info) ->
    [prot_a_integer, [prot_a_integer], [prot_a_aux_item]];
make_args(query_async) ->
    [];
make_args(query_predefined_aux_items) ->
    [];
make_args(query_read_texts) ->
    [prot_a_integer, prot_a_integer, prot_a_bool, prot_a_integer];
make_args(send_message) ->
    [prot_a_integer, prot_a_string];
make_args(set_client_version) ->
    [prot_a_string, prot_a_string];
make_args(set_conf_type) ->
    [prot_a_integer, prot_a_extended_conf];
make_args(set_connection_time_format) ->
    [prot_a_bool];
make_args(set_etc_motd) ->
    [prot_a_integer, prot_a_integer];
make_args(set_garb_nice) ->
    [prot_a_integer, prot_a_integer];
make_args(set_info) ->
    [prot_a_integer, prot_a_integer, prot_a_integer, prot_a_integer, prot_a_integer, prot_a_integer];
make_args(set_keep_commented) ->
    [prot_a_integer, prot_a_integer];
make_args(set_last_read) ->
    [prot_a_integer, prot_a_integer];
make_args(set_membership_type) ->
    [prot_a_integer, prot_a_integer, prot_a_membership_type];
make_args(set_motd_of_lyskom) ->
    [prot_a_integer];
make_args(set_passwd) ->
    [prot_a_integer, prot_a_string, prot_a_string];
make_args(set_permitted_submitters) ->
    [prot_a_integer, prot_a_integer];
make_args(set_pers_flags) ->
    [prot_a_integer, prot_a_personal_flags];
make_args(set_presentation) ->
    [prot_a_integer, prot_a_integer];
make_args(set_priv_bits) ->
    [prot_a_integer, prot_a_priv_bits];
make_args(set_read_ranges) ->
    [prot_a_integer, [prot_a_read_range]];
make_args(set_super_conf) ->
    [prot_a_integer, prot_a_integer];
make_args(set_supervisor) ->
    [prot_a_integer, prot_a_integer];
make_args(set_unread) ->
    [prot_a_integer, prot_a_integer];
make_args(set_user_area) ->
    [prot_a_integer, prot_a_integer];
make_args(shutdown_kom) ->
    [prot_a_integer];
make_args(sub_comment) ->
    [prot_a_integer, prot_a_integer];
make_args(sub_footnote) ->
    [prot_a_integer, prot_a_integer];
make_args(sub_member) ->
    [prot_a_integer, prot_a_integer];
make_args(sub_recipient) ->
    [prot_a_integer, prot_a_integer];
make_args(sync_kom) ->
    [];
make_args(unmark_text) ->
    [prot_a_integer];
make_args(user_active) ->
    [];
make_args(who_am_i) ->
    [].

%%% How to turn returned lists from calls into something useful
-spec response(atom(), list()) -> any().
response(create_anonymous_text, Args) ->
    one_arg(prot_a_integer, Args);
response(create_conf, Args) ->
    one_arg(prot_a_integer, Args);
response(create_person, Args) ->
    one_arg(prot_a_integer, Args);
response(create_text, Args) ->
    one_arg(prot_a_integer, Args);
response(find_next_conf_no, Args) ->
    one_arg(prot_a_integer, Args);
response(find_next_text_no, Args) ->
    one_arg(prot_a_integer, Args);
response(find_previous_conf_no, Args) ->
    one_arg(prot_a_integer, Args);
response(find_previous_text_no, Args) ->
    one_arg(prot_a_integer, Args);
response(first_unused_conf_no, Args) ->
    one_arg(prot_a_integer, Args);
response(first_unused_text_no, Args) ->
    one_arg(prot_a_integer, Args);
response(get_client_name, Args) ->
    one_arg(prot_a_string, Args);
response(get_client_version, Args) ->
    one_arg(prot_a_string, Args);
response(get_collate_table, Args) ->
    one_arg(prot_a_string, Args);
response(get_last_text, Args) ->
    one_arg(prot_a_integer, Args);
response(get_unread_confs, Args) ->
    one_arg([prot_a_integer], Args);
response(query_async, Args) ->
    one_arg([prot_a_integer], Args);
response(query_predefined_aux_items, Args) ->
    one_arg([prot_a_integer], Args);
response(who_am_i, Args) ->
    one_arg(prot_a_integer, Args);
response(re_z_lookup, Args) ->
    one_arg([prot_a_conf_z_info], Args);
response(lookup_z_name, Args) ->
    one_arg([prot_a_conf_z_info], Args);
response(who_is_on_dynamic, Args) ->
    one_arg([prot_a_dynamic_session_info], Args);
response(get_text_stat, Args) ->
    one_arg(prot_a_textstat, Args);
response(get_time, Args) ->
    one_arg(prot_a_time, Args);
response(get_text, Args) ->
    one_arg(prot_a_data, Args);
response(get_uconf_stat, Args) ->
    one_arg(prot_a_uconference, Args);
response(get_conf_stat, Args) ->
    one_arg(prot_a_conference, Args);
response(get_info, Args) ->
    {ok,
        prot_a_args:parse(
            [
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                [prot_a_aux_item]
            ],
            Args
        )};
response(get_marks, Args) ->
    one_arg([prot_a_mark], Args);
response(get_members, Args) ->
    one_arg([prot_a_member], Args);
response(get_membership, Args) ->
    one_arg(prot_a_membership, Args);
response(get_person_stat, Args) ->
    one_arg(prot_a_person, Args);
response(get_static_session_info, Args) ->
    {ok, prot_a_args:parse([prot_a_string, prot_a_string, prot_a_string, prot_a_time], Args)};
response(get_stats, Args) ->
    one_arg([prot_a_stats], Args);
response(get_stats_description, Args) ->
    one_arg(prot_a_stats_description, Args);
response(get_version_info, Args) ->
    {ok, prot_a_args:parse([prot_a_integer, prot_a_string, prot_a_string], Args)};
response(local_to_global, Args) ->
    one_arg([prot_a_text_mapping], Args);
response(local_to_global_reverse, Args) ->
    one_arg([prot_a_text_mapping], Args);
response(map_created_texts, Args) ->
    one_arg([prot_a_text_mapping], Args);
response(map_created_texts_reverse, Args) ->
    one_arg([prot_a_text_mapping], Args);
response(query_read_texts, Args) ->
    one_arg([prot_a_membership], Args);
response(get_boottime_info, Args) ->
    {ok,
        prot_a_args:parse(
            [
                prot_a_time,
                prot_a_time,
                prot_a_string,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer
            ],
            Args
        )};
response(_CallName, []) ->
    ok.

%%% Helpers
one_arg(Type, List) ->
    [Arg] = prot_a_args:parse([Type], List),
    {ok, Arg}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accept_async_test() ->
    Res = ?l2b(make(accept_async, lists:seq(2, 10, 2))),
    ?assertEqual(<<"80 5 { 2 4 6 8 10 }">>, Res).

-endif.
