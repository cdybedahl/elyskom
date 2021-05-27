%%%-------------------------------------------------------------------
%% @doc elyskom public API
%% @end
%%%-------------------------------------------------------------------

-module(elyskom_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    elyskom_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
