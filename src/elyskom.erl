-module(elyskom).

-behaviour(gen_statem).

-export([callback_mode/0]).
-export([init/1]).
-export([start_link/0]).
-export([connecting/3]).
-export([handshake/3]).
-export([waiting/3]).
-export([token/3]).
-export([message/3]).

-define(INITIAL_DATA,
        #{delay => 1,
          port => undef,
          stream_acc => <<>>,
          token_acc => <<>>,
          tokens => []}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, no_args, []).

init(_Args) ->
    {ok, connecting, ?INITIAL_DATA, [{next_event, internal, startup}]}.

connecting(_Type, startup, Data) ->
    case gen_tcp:connect("kom.lysator.liu.se", 4894, [binary, inet, {active, once}]) of
        {ok, Port} ->
            io:format("Connected.~n"),
            gen_tcp:send(Port, <<"A5Hcalle\n">>),
            {next_state, handshake, maps:put(port, Port, Data)};
        _ ->
            Delay = maps:get(delay, Data),
            NewDelay = erlang:min(300, 2 * Delay),
            io:format("Connecting failed. Delaying ~p seconds.~n", [Delay]),
            {keep_state, maps:put(delay, NewDelay, Data), [{timeout, Delay * 1000, startup}]}
    end.

handshake(info, {tcp, Port, <<"LysKOM\n">>}, #{port := Port} = Data) ->
    inet:setopts(Port, [{active, once}]),
    {next_state, waiting, Data}.

waiting(info, {tcp, Port, Payload}, #{port := Port} = Data) ->
    inet:setopts(Port, [{active, once}]),
    case Payload of
        <<":", Rest/binary>> ->
            io:format("Async: ~p~n", [Rest]),
            {next_state,
             token,
             append_to_stream(Rest, add_token(async, Data)),
             [{next_event, internal, tokenize}]};
        Other ->
            io:format("Other: ~p~n", [Other]),
            keep_state_and_data
    end;
waiting(Type, Content, Data) ->
    io:format("waiting: ~p ~p ~p~n", [Type, Content, Data]),
    keep_state_and_data.

token(internal, tokenize, #{stream_acc := <<>>}) ->
    keep_state_and_data;
token(internal, tokenize, Data) ->
    Stream = maps:get(stream_acc, Data),
    %% TODO: Handle holleriths
    case Stream of
        <<' ', Rest/binary>> ->
            NewData1 = add_token(maps:get(token_acc, Data), Data),
            NewData2 = maps:put(token_acc, <<>>, NewData1),
            {next_state,
             message,
             maps:put(stream_acc, Rest, NewData2),
             [{next_event, internal, tokenize}]};
        <<'\n', Rest/binary>> ->
            io:format("Message: ~p~n",
                      [lists:reverse([maps:get(token_acc, Data) | maps:get(tokens, Data)])]),
            NewData1 = maps:put(token_acc, <<>>, Data),
            NewData2 = maps:put(tokens, [], NewData1),
            {next_state,
             message,
             maps:put(stream_acc, Rest, NewData2),
             [{next_event, internal, tokenize}]};
        <<Char:8, Rest/binary>> ->
            NewData1 = maps:put(token_acc, <<(maps:get(token_acc, Data)), Char>>, Data),
            {next_state,
             message,
             maps:put(stream_acc, Rest, NewData1),
             [{next_event, internal, tokenize}]}
    end;
token(Type, Content, Data) ->
    io:format("token: ~p ~p ~p~n", [Type, Content, Data]),
    keep_state_and_data.

message(Type, Content, Data) ->
    io:format("message: ~p ~p ~p~n", [Type, Content, Data]),
    keep_state_and_data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Help functions
add_token(Token, Map) ->
    maps:put(tokens, [Token | maps:get(tokens, Map)], Map).

append_to_stream(Data, Map) ->
    maps:put(stream_acc, <<(maps:get(stream_acc, Map))/binary, Data/binary>>, Map).
