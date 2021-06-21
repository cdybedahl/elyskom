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
-export([who_am_i/1]).
-export([first_unused_conf_no/1]).
-export([first_unused_text_no/1]).
-export([get_marks/1]).
-export([get_version_info/1]).
-export([logout/1]).
-export([query_async/1]).
-export([query_predefined_aux_items/1]).
-export([sync_kom/1]).
-export([user_active/1]).
-export([get_stats_description/1]).
-export([get_collate_table/1]).
-export([get_boottime_info/1]).
-export([get_info/1]).

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

-spec who_is_on_dynamic(pid(), boolean(), boolean(), non_neg_integer()) ->
    [prot_a_dynamic_session_info:t()] | prot_a_error:t().
who_is_on_dynamic(Pid, WantVisible, WantInvisible, ActiveLast) ->
    gen_statem:call(Pid, [who_is_on_dynamic, WantVisible, WantInvisible, ActiveLast]).

-spec lookup_z_name(pid(), prot_a_string:t(), boolean(), boolean()) ->
    [prot_a_extended_conf:any_conf()] | prot_a_error:t().
lookup_z_name(Pid, Name, WantPers, WantConfs) ->
    gen_statem:call(Pid, [lookup_z_name, Name, WantPers, WantConfs]).

-spec re_z_lookup(pid(), prot_a_string:t(), boolean(), boolean()) ->
    [prot_a_extended_conf:any_conf()] | prot_a_error:t().
re_z_lookup(Pid, Name, WantPers, WantConfs) ->
    gen_statem:call(Pid, [re_z_lookup, Name, WantPers, WantConfs]).

-spec add_member(pid(), pos_integer(), pos_integer(), pos_integer(), pos_integer(), prot_a_membership_type:t()) ->
    ok | prot_a_error:t().
add_member(Pid, ConfNo, PersNo, Priority, Where, Type) ->
    gen_statem:call(Pid, [add_member, ConfNo, PersNo, Priority, Where, Type]).

-spec add_recipient(pid(), pos_integer(), pos_integer(), prot_a_info:t()) -> ok | prot_a_error:t().
add_recipient(Pid, TextNo, ConfNo, Type) ->
    gen_statem:call(Pid, [add_recipient, TextNo, ConfNo, Type]).

-spec create_text(pid(), binary(), [prot_a_misc_info:t()], [prot_a_aux_item:input()]) ->
    pos_integer() | prot_a_error:t().
create_text(Pid, Text, MiscInfos, AuxItems) ->
    gen_statem:call(Pid, [create_text, Text, MiscInfos, AuxItems]).

-spec create_anonymous_text(pid(), binary(), [prot_a_misc_info:t()], [prot_a_aux_item:input()]) ->
    pos_integer() | prot_a_error:t().
create_anonymous_text(Pid, Text, MiscInfos, AuxItems) ->
    gen_statem:call(Pid, [create_anonymous_text, Text, MiscInfos, AuxItems]).

-spec create_conf(pid(), binary(), prot_a_extended_conf:any_conf(), [prot_a_aux_item:t()]) ->
    pos_integer() | prot_a_error:t().
create_conf(Pid, Name, ConfType, AuxItems) ->
    gen_statem:call(Pid, [create_conf, Name, ConfType, AuxItems]).

-spec create_person(pid(), binary(), binary(), prot_a_personal_flags:t(), [prot_a_aux_item:t()]) ->
    pos_integer() | prot_a_error:t().
create_person(Pid, Name, Passwd, PersonalFlags, AuxItems) ->
    gen_statem:call(Pid, [create_person, Name, Passwd, PersonalFlags, AuxItems]).

%% Generated:ish
-spec add_comment(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
add_comment(Pid, TextNo, CommentTo) -> gen_statem:call(Pid, [add_comment, TextNo, CommentTo]).

-spec add_footnote(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
add_footnote(Pid, TextNo, FootnoteTo) ->
    gen_statem:call(Pid, [add_footnote, TextNo, FootnoteTo]).

-spec change_conference(pid(), pos_integer()) -> ok | prot_a_error:t().
change_conference(Pid, ConfNo) -> gen_statem:call(Pid, [change_conference, ConfNo]).

-spec change_name(pid(), pos_integer(), binary()) -> ok | prot_a_error:t().
change_name(Pid, PersNo, Name) -> gen_statem:call(Pid, [change_name, PersNo, Name]).

-spec change_what_i_am_doing(pid(), binary()) -> ok | prot_a_error:t().
change_what_i_am_doing(Pid, What) -> gen_statem:call(Pid, [change_what_i_am_doing, What]).

-spec delete_conf(pid(), pos_integer()) -> ok | prot_a_error:t().
delete_conf(Pid, ConfNo) -> gen_statem:call(Pid, [delete_conf, ConfNo]).

-spec delete_text(pid(), pos_integer()) -> ok | prot_a_error:t().
delete_text(Pid, TextNo) -> gen_statem:call(Pid, [delete_text, TextNo]).

-spec disconnect(pid(), pos_integer()) -> ok | prot_a_error:t().
disconnect(Pid, SessionNo) -> gen_statem:call(Pid, [disconnect, SessionNo]).

-spec enable(pid(), byte()) -> ok | prot_a_error:t().
enable(Pid, Level) -> gen_statem:call(Pid, [enable, Level]).

-spec find_next_conf_no(pid(), pos_integer()) -> pos_integer() | prot_a_error:t().
find_next_conf_no(Pid, Start) -> gen_statem:call(Pid, [find_next_conf_no, Start]).

-spec find_next_text_no(pid(), pos_integer()) -> pos_integer() | prot_a_error:t().
find_next_text_no(Pid, Start) -> gen_statem:call(Pid, [find_next_text_no, Start]).

-spec find_previous_conf_no(pid(), pos_integer()) -> pos_integer() | prot_a_error:t().
find_previous_conf_no(Pid, Start) -> gen_statem:call(Pid, [find_previous_conf_no, Start]).

-spec find_previous_text_no(pid(), pos_integer()) -> pos_integer() | prot_a_error:t().
find_previous_text_no(Pid, Start) -> gen_statem:call(Pid, [find_previous_text_no, Start]).

-spec get_client_name(pid(), pos_integer()) -> prot_a_string:t() | prot_a_error:t().
get_client_name(Pid, SessionNo) -> gen_statem:call(Pid, [get_client_name, SessionNo]).

-spec get_client_version(pid(), pos_integer()) -> prot_a_string:t() | prot_a_error:t().
get_client_version(Pid, SessionNo) -> gen_statem:call(Pid, [get_client_version, SessionNo]).

-spec get_conf_stat(pid(), pos_integer()) -> prot_a_conference:t() | prot_a_error:t().
get_conf_stat(Pid, ConfNo) -> gen_statem:call(Pid, [get_conf_stat, ConfNo]).

-spec get_last_text(pid(), prot_a_time:t()) -> pos_integer() | prot_a_error:t().
get_last_text(Pid, When) -> gen_statem:call(Pid, [get_last_text, When]).

-spec get_members(pid(), pos_integer(), pos_integer(), pos_integer()) -> [prot_a_member:t()] | prot_a_error:t().
get_members(Pid, ConfNo, First, NoOfMembers) ->
    gen_statem:call(Pid, [get_members, ConfNo, First, NoOfMembers]).

-spec get_membership(pid(), pos_integer(), pos_integer(), pos_integer(), boolean(), pos_integer()) ->
    [prot_a_membership:t()] | prot_a_error:t().
get_membership(Pid, PersNo, First, NoOfConf, WantReadRanges, MaxRanges) ->
    gen_statem:call(Pid, [get_membership, PersNo, First, NoOfConf, WantReadRanges, MaxRanges]).

-spec get_person_stat(pid(), pos_integer()) -> prot_a_person:t() | prot_a_error:t().
get_person_stat(Pid, PersNo) -> gen_statem:call(Pid, [get_person_stat, PersNo]).

-spec get_static_session_info(pid(), pos_integer()) -> [prot_a_string:t() | prot_a_time:t()].
get_static_session_info(Pid, SessionNo) -> gen_statem:call(Pid, [get_static_session_info, SessionNo]).

-spec get_stats(pid(), prot_a_string:t()) -> [prot_a_stats:t()] | prot_a_error:t().
get_stats(Pid, What) -> gen_statem:call(Pid, [get_stats, What]).

-spec get_unread_confs(pid(), pos_integer()) -> [pos_integer()] | prot_a_error:t().
get_unread_confs(Pid, PersNo) -> gen_statem:call(Pid, [get_unread_confs, PersNo]).

-spec local_to_global(pid(), pos_integer(), pos_integer(), pos_integer()) ->
    prot_a_text_mapping:t() | prot_a_error:t().
local_to_global(Pid, ConfNo, FirstLocalNo, NoOfExistingTexts) ->
    gen_statem:call(Pid, [local_to_global, ConfNo, FirstLocalNo, NoOfExistingTexts]).

-spec local_to_global_reverse(pid(), pos_integer(), pos_integer(), pos_integer()) ->
    prot_a_text_mapping:t() | prot_a_error:t().
local_to_global_reverse(Pid, ConfNo, LocalNoCeiling, NoOfExistingTexts) ->
    gen_statem:call(Pid, [local_to_global_reverse, ConfNo, LocalNoCeiling, NoOfExistingTexts]).

-spec map_created_texts(pid(), pos_integer(), pos_integer(), pos_integer()) ->
    prot_a_text_mapping:t() | prot_a_error:t().
map_created_texts(Pid, PersNo, FirstLocalNo, NoOfExistingTexts) ->
    gen_statem:call(Pid, [map_created_texts, PersNo, FirstLocalNo, NoOfExistingTexts]).

-spec map_created_texts_reverse(pid(), pos_integer(), pos_integer(), pos_integer()) ->
    prot_a_text_mapping:t() | prot_a_error:t().
map_created_texts_reverse(Pid, PersNo, LocalNoCeiling, NoOfExistingTexts) ->
    gen_statem:call(Pid, [map_created_texts_reverse, PersNo, LocalNoCeiling, NoOfExistingTexts]).

-spec mark_as_read(pid(), pos_integer(), [pos_integer()]) -> ok | prot_a_error:t().
mark_as_read(Pid, ConfNo, LocalTextNoArray) ->
    gen_statem:call(Pid, [mark_as_read, ConfNo, LocalTextNoArray]).

-spec mark_as_unread(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
mark_as_unread(Pid, ConfNo, TextNo) ->
    gen_statem:call(Pid, [mark_as_unread, ConfNo, TextNo]).

-spec mark_text(pid(), pos_integer(), byte()) -> ok | prot_a_error:t().
mark_text(Pid, TextNo, Type) when Type >= 0 andalso Type < 256 ->
    gen_statem:call(Pid, [mark_text, TextNo, Type]).

-spec modify_conf_info(pid(), pos_integer(), [pos_integer()], [prot_a_aux_item:input()]) ->
    ok | prot_a_error:t().
modify_conf_info(Pid, ConfNo, DeleteList, AddAuxItemsList) ->
    gen_statem:call(Pid, [modify_conf_info, ConfNo, DeleteList, AddAuxItemsList]).

-spec modify_system_info(pid(), [pos_integer()], [prot_a_aux_item:input()]) -> ok | prot_a_error:t().
modify_system_info(Pid, ItemsToDelete, AuxItemsToAdd) ->
    gen_statem:call(Pid, [modify_system_info, ItemsToDelete, AuxItemsToAdd]).

-spec modify_text_info(pid(), pos_integer(), [pos_integer()], [prot_a_aux_item:input()]) ->
    ok | prot_a_error:t().
modify_text_info(Pid, TextNo, ItemsToDelete, AuxItemsToAdd) ->
    gen_statem:call(Pid, [modify_text_info, TextNo, ItemsToDelete, AuxItemsToAdd]).

-spec query_read_texts(pid(), pos_integer(), pos_integer(), boolean(), pos_integer()) ->
    prot_a_membership:t() | prot_a_error:t().
query_read_texts(Pid, PersNo, ConfNo, WantReadRanges, MaxRanges) ->
    gen_statem:call(Pid, [query_read_texts, PersNo, ConfNo, WantReadRanges, MaxRanges]).

-spec send_message(pid(), pos_integer(), prot_a_string:t()) -> ok | prot_a_error:t().
send_message(Pid, PersNo, Message) -> gen_statem:call(Pid, [send_message, PersNo, Message]).

-spec set_client_version(pid(), prot_a_string:t(), prot_a_string:t()) -> ok | prot_a_error:t().
set_client_version(Pid, Name, Version) ->
    gen_statem:call(Pid, [set_client_version, Name, Version]).

-spec set_conf_type(pid(), pos_integer(), prot_a_extended_conf:any_conf()) -> ok | prot_a_error:t().
set_conf_type(Pid, ConfNo, Type) ->
    gen_statem:call(Pid, [set_conf_type, ConfNo, Type]).

-spec set_connection_time_format(pid(), boolean()) -> ok | prot_a_error:t().
set_connection_time_format(Pid, UseUTC) -> gen_statem:call(Pid, [set_connection_time_format, UseUTC]).

-spec set_etc_motd(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_etc_motd(Pid, ConfNo, TextNo) ->
    gen_statem:call(Pid, [set_etc_motd, ConfNo, TextNo]).

-spec set_garb_nice(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_garb_nice(Pid, ConfNo, Nice) ->
    gen_statem:call(Pid, [set_garb_nice, ConfNo, Nice]).

-spec set_info(pid(), pos_integer(), pos_integer(), pos_integer(), pos_integer(), pos_integer(), pos_integer()) ->
    ok | prot_a_error:t().
set_info(Pid, Version, ConfPresConf, PersPresConf, MotdConf, KomNewsConf, MotdOfLyskom) ->
    gen_statem:call(Pid, [set_info, Version, ConfPresConf, PersPresConf, MotdConf, KomNewsConf, MotdOfLyskom]).

-spec set_keep_commented(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_keep_commented(Pid, ConfNo, KeepCommented) ->
    gen_statem:call(Pid, [set_keep_commented, ConfNo, KeepCommented]).

-spec set_last_read(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_last_read(Pid, ConfNo, LastRead) ->
    gen_statem:call(Pid, [set_last_read, ConfNo, LastRead]).

-spec set_membership_type(pid(), pos_integer(), pos_integer(), prot_a_membership_type:t()) ->
    ok | prot_a_error:t().
set_membership_type(Pid, PersNo, ConfNo, MembershipType) ->
    gen_statem:call(Pid, [set_membership_type, PersNo, ConfNo, MembershipType]).

-spec set_motd_of_lyskom(pid(), pos_integer()) -> prot_a_error:t().
set_motd_of_lyskom(Pid, TextNo) -> gen_statem:call(Pid, [set_motd_of_lyskom, TextNo]).

-spec set_passwd(pid(), pos_integer(), prot_a_string:t(), prot_a_string:t()) ->
    ok | prot_a_error:t().
set_passwd(Pid, PersNo, OldPass, NewPass) ->
    gen_statem:call(Pid, [set_passwd, PersNo, OldPass, NewPass]).

-spec set_permitted_submitters(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_permitted_submitters(Pid, ConfNo, PermittedConf) ->
    gen_statem:call(Pid, [set_permitted_submitters, ConfNo, PermittedConf]).

-spec set_pers_flags(pid(), pos_integer(), prot_a_personal_flags:t()) -> ok | prot_a_error:t().
set_pers_flags(Pid, PersNo, PersonalFlags) ->
    gen_statem:call(Pid, [set_pers_flags, PersNo, PersonalFlags]).

-spec set_presentation(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_presentation(Pid, ConfNo, TextNo) ->
    gen_statem:call(Pid, [set_presentation, ConfNo, TextNo]).

-spec set_priv_bits(pid(), pos_integer(), prot_a_priv_bits:t()) -> ok | prot_a_error:t().
set_priv_bits(Pid, PersNo, PrivBits) ->
    gen_statem:call(Pid, [set_priv_bits, PersNo, PrivBits]).

-spec set_read_ranges(pid(), pos_integer(), [prot_a_read_range:t()]) -> ok | prot_a_error:t().
set_read_ranges(Pid, ConfNo, ReadRangeArray) ->
    gen_statem:call(Pid, [set_read_ranges, ConfNo, ReadRangeArray]).

-spec set_super_conf(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_super_conf(Pid, ConfNo, SuperConfNo) ->
    gen_statem:call(Pid, [set_super_conf, ConfNo, SuperConfNo]).

-spec set_supervisor(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_supervisor(Pid, ConfNo, AdminNo) ->
    gen_statem:call(Pid, [set_supervisor, ConfNo, AdminNo]).

-spec set_unread(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_unread(Pid, ConfNo, NoOfUnread) -> gen_statem:call(Pid, [set_unread, ConfNo, NoOfUnread]).

-spec set_user_area(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
set_user_area(Pid, PersNo, UserAreaTextNo) ->
    gen_statem:call(Pid, [set_user_area, PersNo, UserAreaTextNo]).

-spec shutdown_kom(pid(), byte()) -> ok | prot_a_error:t().
shutdown_kom(Pid, ExitVal) -> gen_statem:call(Pid, [shutdown_kom, ExitVal]).

-spec sub_comment(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
sub_comment(Pid, TextNo, CommentTo) -> gen_statem:call(Pid, [sub_comment, TextNo, CommentTo]).

-spec sub_footnote(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
sub_footnote(Pid, TextNo, FootnoteTo) ->
    gen_statem:call(Pid, [sub_footnote, TextNo, FootnoteTo]).

-spec sub_member(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
sub_member(Pid, ConfNo, PersNo) -> gen_statem:call(Pid, [sub_member, ConfNo, PersNo]).

-spec sub_recipient(pid(), pos_integer(), pos_integer()) -> ok | prot_a_error:t().
sub_recipient(Pid, TextNo, ConfNo) ->
    gen_statem:call(Pid, [sub_recipient, TextNo, ConfNo]).

-spec unmark_text(pid(), pos_integer()) -> ok | prot_a_error:t().
unmark_text(Pid, TextNo) -> gen_statem:call(Pid, [unmark_text, TextNo]).

-spec who_am_i(pid()) -> pos_integer() | prot_a_error:t().
who_am_i(Pid) -> gen_statem:call(Pid, [who_am_i]).

-spec first_unused_conf_no(pid()) -> pos_integer() | prot_a_error:t().
first_unused_conf_no(Pid) -> gen_statem:call(Pid, [first_unused_conf_no]).

-spec first_unused_text_no(pid()) -> pos_integer() | prot_a_error:t().
first_unused_text_no(Pid) -> gen_statem:call(Pid, [first_unused_text_no]).

-spec get_marks(pid()) -> [prot_a_mark:t()] | prot_a_error:t().
get_marks(Pid) -> gen_statem:call(Pid, [get_marks]).

-spec get_version_info(pid()) -> [pos_integer()] | prot_a_error:t().
get_version_info(Pid) -> gen_statem:call(Pid, [get_version_info]).

-spec logout(pid()) -> ok | prot_a_error:t().
logout(Pid) -> gen_statem:call(Pid, [logout]).

-spec query_async(pid()) -> [pos_integer()] | prot_a_error:t().
query_async(Pid) -> gen_statem:call(Pid, [query_async]).

-spec query_predefined_aux_items(pid()) -> [pos_integer()] | prot_a_error:t().
query_predefined_aux_items(Pid) -> gen_statem:call(Pid, [query_predefined_aux_items]).

-spec sync_kom(pid()) -> ok | prot_a_error:t().
sync_kom(Pid) -> gen_statem:call(Pid, [sync_kom]).

-spec user_active(pid()) -> ok | prot_a_error:t().
user_active(Pid) -> gen_statem:call(Pid, [user_active]).

-spec get_stats_description(pid()) -> prot_a_stats_description:t() | prot_a_error:t().
get_stats_description(Pid) -> gen_statem:call(Pid, [get_stats_description]).

-spec get_collate_table(pid()) -> prot_a_string:t().
get_collate_table(Pid) -> gen_statem:call(Pid, [get_collate_table]).

-spec get_boottime_info(pid()) -> [prot_a_time:t() | prot_a_string:t() | integer()].
get_boottime_info(Pid) -> gen_statem:call(Pid, [get_boottime_info]).

-spec get_info(pid()) -> [integer() | [prot_a_aux_item:t()]].
get_info(Pid) -> gen_statem:call(Pid, [get_info]).

%% Missing calls

%% get_info, no args, returns list of stuff
