-module(elyskom).

-export([login/3]).
-export([login/4]).

login(Pid, UserNo, Password) ->
    login(Pid, UserNo, Password, false).

login(Pid, UserNo, Password, Invisible) ->
    gen_statem:call(Pid, [login, UserNo, Password, Invisible]).
