-module(elyskom).

-include("elyskom.hrl").

-export([new/0, new/1, new/2]).
-export([login/3, login/4]).
-export([accept_async/2]).
-export([get_uconf_stat/2]).
-export([get_text/4]).
-export([get_time/1]).

new() ->
    new("kom.lysator.liu.se", ?TCP_PORT).
new(Host) ->
    new(Host, ?TCP_PORT).
new(Host, TcpPort) ->
    {ok, Pid} = elyskom_socket:start_link(Host, TcpPort),
    receive
        {elyskom, connected} -> {ok, Pid}
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

get_time(Pid) ->
    gen_statem:call(Pid, [get_time]).
