-module(elyskom).

-export([new/0, new/1, new/2]).
-export([login/3, login/4]).

new() ->
    new("kom.lysator.liu.se", 4894).
new(Host) ->
    new(Host, 4894).
new(Host, TcpPort) ->
    elyskom_socket:start_link(Host, TcpPort).

login(Pid, UserNo, Password) ->
    login(Pid, UserNo, Password, false).

login(Pid, UserNo, Password, Invisible) ->
    gen_statem:call(Pid, [login, UserNo, Password, Invisible]).
