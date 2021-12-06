%% @hidden
-module(elyskom_socket).

-behaviour(gen_statem).

-export([callback_mode/0]).
-export([init/1]).
-export([start_link/2]).
-export([start/2]).
-export([connecting/3]).
-export([handshake/3]).
-export([waiting/3]).
-export([token/3]).
-export([hollerith/3]).

-define(INITIAL_DATA, #{
    delay => 1,
    port => undef,
    stream_acc => <<>>,
    token_acc => <<>>,
    tokens => [],
    hostname => "",
    tcp_port => 0,
    pending => null,
    call_counter => 1,
    peer => null
}).

-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) ->
        handle_common(T, C, ?FUNCTION_NAME, D)
).

callback_mode() ->
    state_functions.

start_link(Host, TcpPort) ->
    gen_statem:start_link(?MODULE, {Host, TcpPort, self()}, []).

start(Host, TcpPort) ->
    gen_statem:start(?MODULE, {Host, TcpPort, self()}, []).

init({Host, TcpPort, Peer}) ->
    Data = ?INITIAL_DATA,
    Pending = ets:new(pending, [set, private]),
    {ok, connecting, Data#{hostname := Host, tcp_port := TcpPort, pending := Pending, peer := Peer}, [
        {next_event, internal, startup}
    ]}.

connecting(_Type, startup, #{delay := Delay, hostname := Host, tcp_port := TcpPort} = Data) ->
    case gen_tcp:connect(Host, TcpPort, [binary, inet, {active, once}]) of
        {ok, Port} ->
            gen_tcp:send(Port, <<"A7Helyskom\n">>),
            logger:debug("TCP connection established."),
            {next_state, handshake, Data#{port := Port}};
        _ ->
            NewDelay = erlang:min(300, 2 * Delay),
            logger:error("Connecting failed. Delaying ~p seconds.", [Delay]),
            {keep_state, Data#{delay := NewDelay}, [{timeout, Delay * 1000, startup}]}
    end.

handshake(info, {tcp, Port, <<"LysKOM\n">>}, #{port := Port, peer := Peer} = Data) ->
    inet:setopts(Port, [{active, once}]),
    Peer ! {elyskom, self(), connected},
    logger:info("Handshake done, Protocol A connection established."),
    {next_state, waiting, Data}.

waiting(internal, tokenize, #{stream_acc := <<>>}) ->
    keep_state_and_data;
waiting(internal, tokenize, #{stream_acc := Payload} = Data) ->
    case Payload of
        <<":", Rest/binary>> ->
            {next_state, token, add_token(async, Data#{stream_acc := Rest}), [
                {next_event, internal, tokenize}
            ]};
        <<"=", Rest/binary>> ->
            {next_state, token, add_token(response, Data#{stream_acc := Rest}), [
                {next_event, internal, tokenize}
            ]};
        <<"%", Rest/binary>> ->
            {next_state, token, add_token(error, Data#{stream_acc := Rest}), [
                {next_event, internal, tokenize}
            ]};
        Other ->
            io:format("Other: ~p~n", [Other]),
            keep_state_and_data
    end;
?HANDLE_COMMON.

token(internal, tokenize, #{stream_acc := <<>>}) ->
    keep_state_and_data;
token(internal, tokenize, #{stream_acc := Stream} = Data) ->
    case Stream of
        <<32, Rest/binary>> ->
            NewData1 = add_token(maps:get(token_acc, Data), Data),
            {next_state, token, NewData1#{token_acc := <<>>, stream_acc := Rest}, [
                {next_event, internal, tokenize}
            ]};
        <<10, Rest/binary>> ->
            Message = lists:reverse([maps:get(token_acc, Data) | maps:get(tokens, Data)]),
            handle_message(Message, Data),
            {next_state, waiting,
                Data#{
                    token_acc := <<>>,
                    tokens := [],
                    stream_acc := Rest
                },
                [{next_event, internal, tokenize}]};
        <<$H, Rest/binary>> ->
            {next_state, hollerith, Data#{stream_acc := Rest}, [{next_event, internal, tokenize}]};
        <<Char:8, Rest/binary>> ->
            {next_state, token,
                Data#{
                    stream_acc := Rest,
                    token_acc := <<(maps:get(token_acc, Data))/binary, Char>>
                },
                [{next_event, internal, tokenize}]}
    end;
?HANDLE_COMMON.

hollerith(internal, tokenize, #{stream_acc := <<>>}) ->
    keep_state_and_data;
hollerith(internal, tokenize, #{token_acc := Prefix} = Data) when is_binary(Prefix) ->
    {keep_state, Data#{token_acc := binary_to_integer(Prefix)}, [
        {next_event, internal, tokenize}
    ]};
hollerith(internal, tokenize, #{token_acc := Length, stream_acc := Stream} = Data) when
    is_integer(Length)
->
    case byte_size(Stream) >= Length of
        false ->
            keep_state_and_data;
        true ->
            <<Hollerith:Length/binary, Rest/binary>> = Stream,
            {next_state, token, Data#{token_acc := Hollerith, stream_acc := Rest}, [
                {next_event, internal, tokenize}
            ]}
    end;
?HANDLE_COMMON.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common to all states
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_common(
    {call, From},
    [CallName | Args],
    _Function,
    #{call_counter := Counter, port := Port, pending := Pending} = Data
) ->
    EncodedArgs = elyskom_call:make(CallName, Args),
    RefNo = integer_to_binary(Counter),
    Payload = iolist_to_binary([RefNo, 32] ++ EncodedArgs ++ [10]),
    gen_tcp:send(Port, Payload),
    ets:insert_new(Pending, {RefNo, CallName, From}),
    {keep_state, Data#{call_counter := Counter + 1}};
handle_common(info, {tcp, Port, Payload}, _Function, #{port := Port} = Data) ->
    inet:setopts(Port, [{active, once}]),
    NewData = append_to_stream(Payload, Data),
    logger:debug("Incoming data: ~p", [NewData]),
    {keep_state, NewData, [{next_event, internal, tokenize}]};
handle_common(
    info,
    {tcp_closed, Port},
    _Function,
    #{port := Port, pending := Pending, peer := Peer} = Data
) ->
    logger:warning("TCP connection lost"),
    ets:foldl(
        fun({_, _, From}, noop) ->
            gen_statem:reply(From, {error, disconnected}),
            noop
        end,
        noop,
        Pending
    ),
    NewData = Data#{
        stream_acc := <<>>,
        token_acc := <<>>,
        tokens := []
    },
    Peer ! {elyskom, self(), disconnected},
    {next_state, connecting, NewData, [{next_event, internal, startup}]};
handle_common(Type, Content, FunctionName, Data) ->
    io:format("~p: [~p] ~p~n~p~n", [FunctionName, Type, Content, Data]),
    keep_state_and_data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Help functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_token(Token, #{tokens := Tokens} = Map) ->
    Map#{tokens := [Token | Tokens]}.

append_to_stream(Data, #{stream_acc := Stream} = Map) ->
    Map#{stream_acc := <<Stream/binary, Data/binary>>}.

handle_message([error | Tail], #{pending := Pending}) ->
    prot_a_response:parse(error, Tail, Pending);
handle_message([response | Tail], #{pending := Pending}) ->
    prot_a_response:parse(response, Tail, Pending);
handle_message([async, _ArgCount | Tail], #{peer := Peer}) ->
    Msg = prot_a_async:parse(Tail),
    Peer ! {elyskom, self(), Msg};
handle_message(Message, _Pending) ->
    io:format("Got a message: ~p~n", [Message]).

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
        #{
            stream_acc => <<"5 17 23\n">>,
            tokens => [start],
            token_acc => <<>>
        },

    {next_state, token, DataOut1, _Actions1} = token(internal, tokenize, DataIn),
    ?assertEqual(
        #{
            stream_acc => <<" 17 23\n">>,
            tokens => [start],
            token_acc => <<"5">>
        },
        DataOut1
    ),

    {next_state, token, DataOut2, _Actions2} = token(internal, tokenize, DataOut1),
    ?assertEqual(
        #{
            stream_acc => <<"17 23\n">>,
            tokens => [<<"5">>, start],
            token_acc => <<>>
        },
        DataOut2
    ).

message_end_test() ->
    DataIn =
        #{
            stream_acc => <<"\n">>,
            tokens => [<<"17">>, <<"5">>, start],
            token_acc => <<"23">>
        },

    {next_state, waiting, DataOut1, _Actions1} = token(internal, tokenize, DataIn),
    ?assertEqual(
        #{
            stream_acc => <<>>,
            tokens => [],
            token_acc => <<>>
        },
        DataOut1
    ).

hollerith_test() ->
    DataIn =
        #{
            stream_acc => <<"Foobar\n">>,
            tokens => [],
            token_acc => <<"6">>
        },
    {keep_state, DataOut1} = hollerith(internal, tokenize, DataIn),
    {next_state, tokenize, DataOut2, _Actions} = hollerith(internal, tokenize, DataOut1),
    ?assertEqual(<<"Foobar">>, maps:get(token_acc, DataOut2)).

-endif.
