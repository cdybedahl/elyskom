-module(elyskom).

-behaviour(gen_statem).

-export([callback_mode/0]).
-export([init/1]).
-export([start_link/0]).
-export([connecting/3]).
-export([handshake/3]).
-export([waiting/3]).
-export([token/3]).
-export([hollerith/3]).

-define(INITIAL_DATA,
        #{delay => 1,
          port => undef,
          messages => [],
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
    NewData = append_to_stream(Payload, Data),
    {keep_state, NewData, [{next_event, internal, tokenize}]};
waiting(internal, tokenize, #{stream_acc := <<>>}) ->
    keep_state_and_data;
waiting(internal, tokenize, #{stream_acc := Payload} = Data) ->
    case Payload of
        <<":", Rest/binary>> ->
            io:format("Async: ~p~n", [Rest]),
            {next_state,
             token,
             add_token(async, maps:put(stream_acc, Rest, Data)),
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
        <<32, Rest/binary>> ->
            NewData1 = add_token(maps:get(token_acc, Data), Data),
            NewData2 = maps:put(token_acc, <<>>, NewData1),
            {next_state,
             token,
             maps:put(stream_acc, Rest, NewData2),
             [{next_event, internal, tokenize}]};
        <<10, Rest/binary>> ->
            Message = lists:reverse([maps:get(token_acc, Data) | maps:get(tokens, Data)]),
            io:format("Message: ~p~n", [Message]),
            NewData0 = maps:put(messages, [Message | maps:get(messages, Data)], Data),
            NewData1 = maps:put(token_acc, <<>>, NewData0),
            NewData2 = maps:put(tokens, [], NewData1),
            {next_state,
             waiting,
             maps:put(stream_acc, Rest, NewData2),
             [{next_event, internal, tokenize}]};
        <<$H, Rest/binary>> ->
            {next_state,
             hollerith,
             maps:put(stream_acc, Rest, Data),
             [{next_event, internal, tokenize}]};
        <<Char:8, Rest/binary>> ->
            NewData1 = maps:put(token_acc, <<(maps:get(token_acc, Data))/binary, Char>>, Data),
            {next_state,
             token,
             maps:put(stream_acc, Rest, NewData1),
             [{next_event, internal, tokenize}]}
    end;
token(Type, Content, Data) ->
    io:format("token: ~p ~p ~p~n", [Type, Content, Data]),
    keep_state_and_data.

hollerith(info, {tcp, Port, Payload}, #{port := Port} = Data) ->
    inet:setopts(Port, [{active, once}]),
    NewData = append_to_stream(Payload, Data),
    {keep_state, NewData, [{next_event, internal, tokenize}]};
hollerith(internal, tokenize, #{token_acc := Prefix} = Data) when is_binary(Prefix) ->
    {keep_state, maps:put(token_acc, binary_to_integer(Prefix), Data)};
hollerith(internal, tokenize, #{token_acc := Length, stream_acc := Stream} = Data)
    when is_integer(Length) ->
    case byte_size(Stream) >= Length of
        false ->
            keep_state_and_data;
        true ->
            <<Hollerith:Length/binary, Rest/binary>> = Stream,
            NewData0 = maps:put(token_acc, Hollerith, Data),
            NewData1 = maps:put(stream_acc, Rest, NewData0),
            {keep_state, NewData1, [{next_event, internal, tokenize}]}
    end;
hollerith(Type, Content, Data) ->
    io:format("hollerith: ~p ~p ~p~n", [Type, Content, Data]),
    keep_state_and_data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Help functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_token(Token, Map) ->
    maps:put(tokens, [Token | maps:get(tokens, Map)], Map).

append_to_stream(Data, Map) ->
    maps:put(stream_acc, <<(maps:get(stream_acc, Map))/binary, Data/binary>>, Map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assert(true).

token_stop_test() ->
    ?assertEqual(keep_state_and_data, token(internal, tokenize, #{stream_acc => <<>>})).

tokenize_test() ->
    DataIn =
        #{stream_acc => <<"5 17 23\n">>,
          tokens => [start],
          token_acc => <<>>},

    {next_state, token, DataOut1, _Actions1} = token(internal, tokenize, DataIn),
    ?assertEqual(#{stream_acc => <<" 17 23\n">>,
                   tokens => [start],
                   token_acc => <<"5">>},
                 DataOut1),

    {next_state, token, DataOut2, _Actions2} = token(internal, tokenize, DataOut1),
    ?assertEqual(#{stream_acc => <<"17 23\n">>,
                   tokens => [<<"5">>, start],
                   token_acc => <<>>},
                 DataOut2).

message_end_test() ->
    DataIn =
        #{stream_acc => <<"\n">>,
          tokens => [<<"17">>, <<"5">>, start],
          token_acc => <<"23">>,
          messages => []},

    {next_state, waiting, DataOut1, _Actions1} = token(internal, tokenize, DataIn),
    ?assertEqual(#{stream_acc => <<>>,
                   tokens => [],
                   token_acc => <<>>,
                   messages => [[start, <<"5">>, <<"17">>, <<"23">>]]},
                 DataOut1).

-endif.