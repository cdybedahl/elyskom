%% @doc The primary interface module for the elyskom application.
%% The functions in here are as straightforward translations as possible
%% of the calls described in the Protocol A specification.
-module(elyskom).

-include("elyskom.hrl").

-export([new/0, new/1, new/2]).
-export([login/3, login/4]).
-export([accept_async/2]).
-export([get_uconf_stat/2]).
-export([get_text/4]).
-export([get_time/1]).
-export([get_text_stat/2]).
-export([who_is_on_dynamic/4]).
-export([lookup_z_name/4]).
-export([re_z_lookup/4]).
-export([add_member/6]).
-export([add_recipient/4]).
-export([create_text/4]).
-export([create_anonymous_text/4]).

-export([add_comment/3]).
-export([add_footnote/3]).
-export([change_conference/2]).
-export([change_name/3]).
-export([change_what_i_am_doing/2]).
-export([create_conf/4]).
-export([create_person/5]).
-export([delete_conf/2]).
-export([delete_text/2]).
-export([disconnect/2]).
-export([enable/2]).
-export([find_next_conf_no/2]).
-export([find_next_text_no/2]).
-export([find_previous_conf_no/2]).
-export([find_previous_text_no/2]).
-export([get_client_name/2]).
-export([get_client_version/2]).
-export([get_conf_stat/2]).
-export([get_last_text/2]).
-export([get_members/4]).
-export([get_membership/6]).
-export([get_person_stat/2]).
-export([get_static_session_info/2]).
-export([get_stats/2]).
-export([get_unread_confs/2]).
-export([local_to_global/4]).
-export([local_to_global_reverse/4]).
-export([map_created_texts/4]).
-export([map_created_texts_reverse/4]).
-export([mark_as_read/3]).
-export([mark_as_unread/3]).
-export([mark_text/3]).
-export([modify_conf_info/4]).
-export([modify_system_info/3]).
-export([modify_text_info/4]).
-export([query_read_texts/5]).
-export([send_message/3]).
-export([set_client_version/3]).
-export([set_conf_type/3]).
-export([set_connection_time_format/2]).
-export([set_etc_motd/3]).
-export([set_garb_nice/3]).
-export([set_info/7]).
-export([set_keep_commented/3]).
-export([set_last_read/3]).
-export([set_membership_type/4]).
-export([set_motd_of_lyskom/2]).
-export([set_passwd/4]).
-export([set_permitted_submitters/3]).
-export([set_pers_flags/3]).
-export([set_presentation/3]).
-export([set_priv_bits/3]).
-export([set_read_ranges/3]).
-export([set_super_conf/3]).
-export([set_supervisor/3]).
-export([set_unread/3]).
-export([set_user_area/3]).
-export([shutdown_kom/2]).
-export([sub_comment/3]).
-export([sub_footnote/3]).
-export([sub_member/3]).
-export([sub_recipient/3]).
-export([unmark_text/2]).
-export([who_am_i/2]).

%% @equiv new("kom.lysator.liu.se", 4894)
new() ->
    new("kom.lysator.liu.se", ?TCP_PORT).

%% @equiv new(Host, 4894)
new(Host) ->
    new(Host, ?TCP_PORT).

%% @doc Connect to the LysKOM server at the given host and port.
%% If it takes more than ten seconds to connect to the server and
%% establish communication, the connection will be broken and a
%% timeout error returned.
-spec new(inet:hostname() | inet:hostent(), pos_integer()) -> {ok, pid()} | {error, timeout}.
new(Host, TcpPort) ->
    {ok, Pid} = elyskom_socket:start_link(Host, TcpPort),
    receive
        {elyskom, Pid, connected} -> {ok, Pid}
    after 10000 ->
        gen_statem:stop(Pid),
        {error, timeout}
    end.

%% @doc Visibly log in the given user using the provided password.
-spec login(pid(), pos_integer(), binary()) -> ok | prot_a_error:t().
login(Pid, UserNo, Password) ->
    login(Pid, UserNo, Password, false).

%% @doc Log in the given user using the provided password.
%% If the final argument is true an invisible session will be used.
-spec login(pid(), pos_integer(), binary(), boolean()) -> ok | prot_a_error:t().
login(Pid, UserNo, Password, Invisible) ->
    gen_statem:call(Pid, [login, UserNo, Password, Invisible]).

-spec accept_async(pid(), [pos_integer()]) -> ok | prot_a_error:t().
accept_async(Pid, AsyncList) ->
    gen_statem:call(Pid, [accept_async, AsyncList]).

-spec get_uconf_stat(pid(), pos_integer()) -> prot_a_uconference:t() | prot_a_error:t().
get_uconf_stat(Pid, ConfNo) ->
    gen_statem:call(Pid, [get_uconf_stat, ConfNo]).

-spec get_text(pid(), pos_integer(), pos_integer(), pos_integer()) -> binary() | prot_a_error:t().
get_text(Pid, TextNo, StartPos, EndPos) ->
    gen_statem:call(Pid, [get_text, TextNo, StartPos, EndPos]).

-spec get_text_stat(pid(), pos_integer()) -> prot_a_textstat:t() | prot_a_error:t().
get_text_stat(Pid, TextNo) ->
    gen_statem:call(Pid, [get_text_stat, TextNo]).

-spec get_time(pid()) -> prot_a_time:t().
get_time(Pid) ->
    gen_statem:call(Pid, [get_time]).

who_is_on_dynamic(Pid, WantVisible, WantInvisible, ActiveLast) ->
    gen_statem:call(Pid, [who_is_on_dynamic, WantVisible, WantInvisible, ActiveLast]).

lookup_z_name(Pid, Name, WantPers, WantConfs) ->
    gen_statem:call(Pid, [lookup_z_name, Name, WantPers, WantConfs]).

re_z_lookup(Pid, Name, WantPers, WantConfs) ->
    gen_statem:call(Pid, [re_z_lookup, Name, WantPers, WantConfs]).

add_member(Pid, ConfNo, PersNo, Priority, Where, Type) ->
    gen_statem:call(Pid, [add_member, ConfNo, PersNo, Priority, Where, Type]).

add_recipient(Pid, TextNo, ConfNo, Type) ->
    gen_statem:call(Pid, [add_recipient, TextNo, ConfNo, Type]).

create_text(Pid, Text, MiscInfos, AuxItems) ->
    gen_statem:call(Pid, [create_text, Text, MiscInfos, AuxItems]).

create_anonymous_text(Pid, Text, MiscInfos, AuxItems) ->
    gen_statem:call(Pid, [create_anonymous_text, Text, MiscInfos, AuxItems]).

create_conf(Pid, Name, ConfType, AuxItems) ->
    gen_statem:call(Pid, [create_conf, Name, ConfType, AuxItems]).

create_person(Pid, Name, Passwd, PersonalFlags, AuxItems) ->
    gen_statem:call(Pid, [create_person, Name, Passwd, PersonalFlags, AuxItems]).

%% Generated:ish
add_comment(Pid, TextNo, CommentTo) -> gen_statem:call(Pid, [add_comment, TextNo, CommentTo]).

add_footnote(Pid, TextNo, FootnoteTo) ->
    gen_statem:call(Pid, [add_footnote, TextNo, FootnoteTo]).

change_conference(Pid, ConfNo) -> gen_statem:call(Pid, [change_conference, ConfNo]).

change_name(Pid, PersNo, Name) -> gen_statem:call(Pid, [change_name, PersNo, Name]).

change_what_i_am_doing(Pid, What) -> gen_statem:call(Pid, [change_what_i_am_doing, What]).

delete_conf(Pid, ConfNo) -> gen_statem:call(Pid, [delete_conf, ConfNo]).

delete_text(Pid, TextNo) -> gen_statem:call(Pid, [delete_text, TextNo]).

disconnect(Pid, SessionNo) -> gen_statem:call(Pid, [disconnect, SessionNo]).

enable(Pid, Level) -> gen_statem:call(Pid, [enable, Level]).

find_next_conf_no(Pid, Start) -> gen_statem:call(Pid, [find_next_conf_no, Start]).

find_next_text_no(Pid, Start) -> gen_statem:call(Pid, [find_next_text_no, Start]).

find_previous_conf_no(Pid, Start) -> gen_statem:call(Pid, [find_previous_conf_no, Start]).

find_previous_text_no(Pid, Start) -> gen_statem:call(Pid, [find_previous_text_no, Start]).

get_client_name(Pid, SessionNo) -> gen_statem:call(Pid, [get_client_name, SessionNo]).

get_client_version(Pid, SessionNo) -> gen_statem:call(Pid, [get_client_version, SessionNo]).

get_conf_stat(Pid, ConfNo) -> gen_statem:call(Pid, [get_conf_stat, ConfNo]).

get_last_text(Pid, When) -> gen_statem:call(Pid, [get_last_text, When]).

get_members(Pid, ConfNo, First, NoOfMembers) ->
    gen_statem:call(Pid, [get_members, ConfNo, First, NoOfMembers]).

get_membership(Pid, PersNo, First, NoOfConf, WantReadRanges, MaxRanges) ->
    gen_statem:call(Pid, [get_membership, PersNo, First, NoOfConf, WantReadRanges, MaxRanges]).

get_person_stat(Pid, PersNo) -> gen_statem:call(Pid, [get_person_stat, PersNo]).

get_static_session_info(Pid, SessionNo) -> gen_statem:call(Pid, [get_static_session_info, SessionNo]).

get_stats(Pid, What) -> gen_statem:call(Pid, [get_stats, What]).

get_unread_confs(Pid, PersNo) -> gen_statem:call(Pid, [get_unread_confs, PersNo]).

local_to_global(Pid, ConfNo, FirstLocalNo, NoOfExistingTexts) ->
    gen_statem:call(Pid, [local_to_global, ConfNo, FirstLocalNo, NoOfExistingTexts]).

local_to_global_reverse(Pid, ConfNo, LocalNoCeiling, NoOfExistingTexts) ->
    gen_statem:call(Pid, [local_to_global_reverse, ConfNo, LocalNoCeiling, NoOfExistingTexts]).

map_created_texts(Pid, PersNo, FirstLocalNo, NoOfExistingTexts) ->
    gen_statem:call(Pid, [map_created_texts, PersNo, FirstLocalNo, NoOfExistingTexts]).

map_created_texts_reverse(Pid, PersNo, LocalNoCeiling, NoOfExistingTexts) ->
    gen_statem:call(Pid, [map_created_texts_reverse, PersNo, LocalNoCeiling, NoOfExistingTexts]).

mark_as_read(Pid, ConfNo, LocalTextNoArray) ->
    gen_statem:call(Pid, [mark_as_read, ConfNo, LocalTextNoArray]).

mark_as_unread(Pid, ConfNo, TextNo) ->
    gen_statem:call(Pid, [mark_as_unread, ConfNo, TextNo]).

mark_text(Pid, TextNo, Type) when Type >= 0 andalso Type < 256 ->
    gen_statem:call(Pid, [mark_text, TextNo, Type]).

modify_conf_info(Pid, ConfNo, DeleteList, AddAuxItemsList) ->
    gen_statem:call(Pid, [modify_conf_info, ConfNo, DeleteList, AddAuxItemsList]).

modify_system_info(Pid, ItemsToDelete, AuxItemsToAdd) ->
    gen_statem:call(Pid, [modify_system_info, ItemsToDelete, AuxItemsToAdd]).

modify_text_info(Pid, TextNo, ItemsToDelete, AuxItemsToAdd) ->
    gen_statem:call(Pid, [modify_text_info, TextNo, ItemsToDelete, AuxItemsToAdd]).

query_read_texts(Pid, PersNo, ConfNo, WantReadRanges, MaxRanges) ->
    gen_statem:call(Pid, [query_read_texts, PersNo, ConfNo, WantReadRanges, MaxRanges]).

send_message(Pid, PersNo, Message) -> gen_statem:call(Pid, [send_message, PersNo, Message]).

set_client_version(Pid, Name, Version) ->
    gen_statem:call(Pid, [set_client_version, Name, Version]).

set_conf_type(Pid, ConfNo, Type) ->
    gen_statem:call(Pid, [set_conf_type, ConfNo, Type]).

set_connection_time_format(Pid, UseUTC) -> gen_statem:call(Pid, [set_connection_time_format, UseUTC]).

set_etc_motd(Pid, ConfNo, TextNo) ->
    gen_statem:call(Pid, [set_etc_motd, ConfNo, TextNo]).

set_garb_nice(Pid, ConfNo, Nice) ->
    gen_statem:call(Pid, [set_garb_nice, ConfNo, Nice]).

set_info(Pid, Version, ConfPresConf, PersPresConf, MotdConf, KomNewsConf, MotdOfLyskom) ->
    gen_statem:call(Pid, [set_info, Version, ConfPresConf, PersPresConf, MotdConf, KomNewsConf, MotdOfLyskom]).

set_keep_commented(Pid, ConfNo, KeepCommented) ->
    gen_statem:call(Pid, [set_keep_commented, ConfNo, KeepCommented]).

set_last_read(Pid, ConfNo, LastRead) ->
    gen_statem:call(Pid, [set_last_read, ConfNo, LastRead]).

set_membership_type(Pid, PersNo, ConfNo, MembershipType) ->
    gen_statem:call(Pid, [set_membership_type, PersNo, ConfNo, MembershipType]).

set_motd_of_lyskom(Pid, TextNo) -> gen_statem:call(Pid, [set_motd_of_lyskom, TextNo]).

set_passwd(Pid, PersNo, OldPass, NewPass) ->
    gen_statem:call(Pid, [set_passwd, PersNo, OldPass, NewPass]).

set_permitted_submitters(Pid, ConfNo, PermittedConf) ->
    gen_statem:call(Pid, [set_permitted_submitters, ConfNo, PermittedConf]).

set_pers_flags(Pid, PersNo, PersonalFlags) ->
    gen_statem:call(Pid, [set_pers_flags, PersNo, PersonalFlags]).

set_presentation(Pid, ConfNo, TextNo) ->
    gen_statem:call(Pid, [set_presentation, ConfNo, TextNo]).

set_priv_bits(Pid, PersNo, PrivBits) ->
    gen_statem:call(Pid, [set_priv_bits, PersNo, PrivBits]).

set_read_ranges(Pid, ConfNo, ReadRangeArray) ->
    gen_statem:call(Pid, [set_read_ranges, ConfNo, ReadRangeArray]).

set_super_conf(Pid, ConfNo, SuperConfNo) ->
    gen_statem:call(Pid, [set_super_conf, ConfNo, SuperConfNo]).

set_supervisor(Pid, ConfNo, AdminNo) ->
    gen_statem:call(Pid, [set_supervisor, ConfNo, AdminNo]).

set_unread(Pid, ConfNo, NoOfUnread) -> gen_statem:call(Pid, [set_unread, ConfNo, NoOfUnread]).

set_user_area(Pid, PersNo, UserAreaTextNo) ->
    gen_statem:call(Pid, [set_user_area, PersNo, UserAreaTextNo]).

shutdown_kom(Pid, ExitVal) -> gen_statem:call(Pid, [shutdown_kom, ExitVal]).

sub_comment(Pid, TextNo, CommentTo) -> gen_statem:call(Pid, [sub_comment, TextNo, CommentTo]).

sub_footnote(Pid, TextNo, FootnoteTo) ->
    gen_statem:call(Pid, [sub_footnote, TextNo, FootnoteTo]).

sub_member(Pid, ConfNo, PersNo) -> gen_statem:call(Pid, [sub_member, ConfNo, PersNo]).

sub_recipient(Pid, TextNo, ConfNo) ->
    gen_statem:call(Pid, [sub_recipient, TextNo, ConfNo]).

unmark_text(Pid, TextNo) -> gen_statem:call(Pid, [unmark_text, TextNo]).

who_am_i(Pid, SessionNo) -> gen_statem:call(Pid, [who_am_i, SessionNo]).
