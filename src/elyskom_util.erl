-module(elyskom_util).

-export([text/2]).
-export([unread/2]).
-export([local_to_global/3]).

-define(UNREAD_LIMIT, 1000).

text(Pid, TextNo) ->
    {ok, TS} = elyskom:get_text_stat(Pid, TextNo),
    #{no_of_chars := Size, aux_item := AuxItems} = TS,
    [#{data := Type}] = lists:filter(fun(#{tag := Tag}) -> Tag == 1 end, AuxItems),
    Length = erlang:byte_size(Type),
    {S, L} = binary:match(Type, <<"charset=">>),
    Encoding = binary:part(Type, S + L, Length - (S + L)),
    {ok, Raw} = elyskom:get_text(Pid, TextNo, 0, Size),
    Text = iconv:convert(Encoding, <<"utf-8">>, Raw),
    [Subject, Body] = binary:split(Text, <<"\n">>),
    TS#{subject => Subject, body => Body}.

unread(Pid, UserId) ->
    {ok, ConfList} = elyskom:get_unread_confs(Pid, UserId),
    List = lists:map(fun(C) -> {C, unread_in_conf(Pid, UserId, C)} end, ConfList),
    List.

unread_in_conf(Pid, UserId, ConfNo) ->
    {ok, #{highest_local_no := HighestLocalNo}} = elyskom:get_uconf_stat(Pid, ConfNo),
    {ok, #{read_ranges := ReadRanges0}} = elyskom:query_read_texts(Pid, UserId, ConfNo, true, 0),
    ReadRanges =
        case lists:last(ReadRanges0) of
            {_, HighestLocalNo} ->
                ReadRanges0;
            _ ->
                ReadRanges0 ++ [{HighestLocalNo + 1, HighestLocalNo + 1}]
        end,
    UnreadRanges = invert_read_list(ReadRanges),
    UnreadList = lists:flatten(lists:map(fun({Low, High}) -> lists:seq(Low, High) end, UnreadRanges)),
    case length(UnreadList) > ?UNREAD_LIMIT of
        false ->
            UL = lists:map(fun(L) -> {L, local_to_global(Pid, ConfNo, L)} end, UnreadList),
            {full, UL, 0};
        true ->
            {First, Rest} = lists:split(?UNREAD_LIMIT, UnreadList),
            FL = lists:map(fun(L) -> {L, local_to_global(Pid, ConfNo, L)} end, First),
            {partial, FL, length(Rest)}
    end.

local_to_global(Pid, ConfNo, LocalNo) ->
    Cache = elyskom_socket:get_l2g_cache(Pid),
    case ets:lookup(Cache, {ConfNo, LocalNo}) of
        [{{ConfNo, LocalNo}, GlobalNo}] ->
            GlobalNo;
        [] ->
            {ok, Mapping} = elyskom:local_to_global(Pid, ConfNo, LocalNo, 255),
            cache_block(Pid, ConfNo, Mapping),
            [{{ConfNo, LocalNo}, GlobalNo}] = ets:lookup(Cache, {ConfNo, LocalNo}),
            GlobalNo
    end.

%%%
%%% Internal functions
%%%

cache_block(Pid, ConfNo, #{block := Block, range_begin := Start, range_end := End}) ->
    Cache = elyskom_socket:get_l2g_cache(Pid),
    Exists = lists:map(fun({Local, Global}) -> {{ConfNo, Local}, Global} end, Block),
    ets:insert(Cache, Exists),
    Locals = lists:map(fun({Local, _}) -> Local end, Block),
    NonExistent = lists:seq(Start, End - 1) -- Locals,
    NonExistentBlock = lists:map(fun(L) -> {{ConfNo, L}, undefined} end, NonExistent),
    ets:insert(Cache, NonExistentBlock).

invert_read_list(List) ->
    invert_read_list(List, []).

invert_read_list([], Acc) ->
    lists:reverse(Acc);
invert_read_list([{_M, _N}], Acc) ->
    invert_read_list([], Acc);
invert_read_list([{_, M}, {N, _} = Next | Tail], Acc) when N - M > 0 ->
    invert_read_list([Next | Tail], [{M + 1, N - 1} | Acc]).
