-module(elyskom).

-behaviour(gen_statem).

-export([callback_mode/0]).
-export([init/1]).
-export([start_link/0]).
-export([connecting/3]).
-export([handshake/3]).
-export([waiting/3]).

-define(INITIAL_STATE,
        #{delay => 1,
          port => undef,
          acc => <<>>,
          tokens => []}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, no_args, []).

init(_Args) ->
    {ok, connecting, ?INITIAL_STATE, [{next_event, internal, startup}]}.

connecting(_Type, startup, State) ->
    io:format("Connecting.~n"),
    case gen_tcp:connect("kom.lysator.liu.se", 4894, [binary, inet, {active, once}]) of
        {ok, Port} ->
            io:format("Connected.~n"),
            gen_tcp:send(Port, <<"A5Hcalle\n">>),
            {next_state, handshake, maps:put(port, Port, State)};
        _ ->
            Delay = maps:get(delay, State),
            NewDelay = erlang:min(300, 2 * Delay),
            io:format("Connecting failed. Delaying ~p seconds.~n", [Delay]),
            {keep_state, maps:put(delay, NewDelay, State), [{timeout, Delay * 1000, startup}]}
    end.

handshake(info, {tcp, Port, <<"LysKOM\n">>}, #{port := Port} = State) ->
    inet:setopts(Port, [{active, once}]),
    {next_state, waiting, State}.

waiting(info, {tcp, Port, Payload}, #{port := Port}) ->
    case Payload of
        <<":", Rest/binary>> ->
            io:format("Async: ~p~n", [Rest]);
        Other ->
            io:format("Other: ~p~n", [Other])
    end,
    inet:setopts(Port, [{active, once}]),
    keep_state_and_data;
waiting(Type, Content, Data) ->
    io:format("~p ~p ~p~n", [Type, Content, Data]),
    keep_state_and_data.
