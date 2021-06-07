-module(elyskom).

-include("elyskom.hrl").

-export([new/0, new/1, new/2]).
-export([login/3, login/4]).
-export([accept_async/2]).

new() ->
    new("kom.lysator.liu.se", ?TCP_PORT).
new(Host) ->
    new(Host, ?TCP_PORT).
new(Host, TcpPort) ->
    elyskom_socket:start_link(Host, TcpPort).

login(Pid, UserNo, Password) ->
    login(Pid, UserNo, Password, false).

login(Pid, UserNo, Password, Invisible) ->
    gen_statem:call(Pid, [login, UserNo, Password, Invisible]).

accept_async(Pid, AsyncList) ->
    gen_statem:call(Pid, [accept_async, AsyncList]).
