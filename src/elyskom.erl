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

new() ->
    new("kom.lysator.liu.se", ?TCP_PORT).
new(Host) ->
    new(Host, ?TCP_PORT).
new(Host, TcpPort) ->
    {ok, Pid} = elyskom_socket:start_link(Host, TcpPort),
    receive
        {elyskom, Pid, connected} -> {ok, Pid}
    after 10000 -> {error, timeout}
    end.

login(Pid, UserNo, Password) ->
    login(Pid, UserNo, Password, false).

login(Pid, UserNo, Password, Invisible) ->
    gen_statem:call(Pid, [login, UserNo, Password, Invisible]).

accept_async(Pid, AsyncList) ->
    gen_statem:call(Pid, [accept_async, AsyncList]).

get_uconf_stat(Pid, ConfNo) ->
    gen_statem:call(Pid, [get_uconf_stat, ConfNo]).

get_text(Pid, TextNo, StartPos, EndPos) ->
    gen_statem:call(Pid, [get_text, TextNo, StartPos, EndPos]).

get_text_stat(Pid, TextNo) ->
    gen_statem:call(Pid, [get_text_stat, TextNo]).

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
