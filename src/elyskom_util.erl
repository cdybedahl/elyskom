-module(elyskom_util).

-export([text/2]).
-export([unread/2]).
-export([unread_in_conf/3]).
-export([local_to_global/3]).

-define(UNREAD_LIMIT, 1000).

text(Pid, TextNo) ->
    {ok, TS} = elyskom:get_text_stat(Pid, TextNo),
    #{no_of_chars := Size, aux_item := AuxItems} = TS,
    Encoding =
        case lists:filter(fun(#{tag := Tag}) -> Tag == 1 end, AuxItems) of
            [#{data := Type}] ->
                Length = erlang:byte_size(Type),
                {S, L} = binary:match(Type, <<"charset=">>),
                binary:part(Type, S + L, Length - (S + L));
            [] ->
                <<"iso-8859-1">>
        end,
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
    UnreadList = get_unread_list(Pid, ConfNo, ReadRanges0, HighestLocalNo),
    {Count, With, Without} =
        lists:foldl(
            fun
                (_LocalNo, {Count, WithGlobal, JustLocal}) when Count >= ?UNREAD_LIMIT ->
                    {Count, WithGlobal, JustLocal + 1};
                (LocalNo, {Count, WithGlobal, JustLocal}) ->
                    case local_to_global(Pid, ConfNo, LocalNo) of
                        undefined ->
                            {Count, WithGlobal, JustLocal};
                        N ->
                            {Count + 1, [{LocalNo, N} | WithGlobal], JustLocal}
                    end
            end,
            {0, [], 0},
            UnreadList
        ),
    {Count, lists:reverse(With), Without}.

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

get_unread_list(Pid, ConfNo, [], HighestLocalNo) ->
    {ok, #{first_local_no := FirstLocalNo}} = elyskom:get_conf_stat(Pid, ConfNo),
    lists:seq(FirstLocalNo, HighestLocalNo);
get_unread_list(_Pid, _ConfNo, ReadRanges0, HighestLocalNo) ->
    ReadRanges =
        case lists:last(ReadRanges0) of
            {_, HighestLocalNo} ->
                ReadRanges0;
            _ ->
                ReadRanges0 ++ [{HighestLocalNo + 1, HighestLocalNo + 1}]
        end,
    UnreadRanges = invert_read_list(ReadRanges),
    lists:flatten(lists:map(fun({Low, High}) -> lists:seq(Low, High) end, UnreadRanges)).

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
