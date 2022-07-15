-module(elyskom_util).

-export([text/2]).
-export([unread/2]).
-export([unread_in_conf/3]).
-export([local_to_global/3]).
-export([name/2]).

-define(UNREAD_LIMIT, 1000).

% @doc Fetch both the text status and the text itself for the given global text
% number from the connected server. Also splits off the subject line from the
% body, and makes a reasonable attempt to turn the body into UTF-8.
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

% @doc Return the unread texts for the given user in the connected server. The
% unreads are returned as a list of hashes. Each hash has four keys. `conf_no'
% holds the conference number. `count' holds a count of unread texts the
% definitely exist. `with' holds a list of two-tuples, where the first element
% is a local text number and the second the corresponding global number. The
% number of items in this list is the same as `count'. The `without' key has a
% list with only local numbers, some of which may no longer exist. The reason
% for this splitting is to make it reasonably fast to fetch the unread list even
% for users with very large numbers of potentially unread texts (as in millions
% of them).
unread(Pid, UserId) ->
    {ok, ConfList} = elyskom:get_unread_confs(Pid, UserId),
    List = lists:map(fun(C) -> unread_in_conf(Pid, UserId, C) end, ConfList),
    List.

% @doc Return the unread hash (as described above) for a specific conference.
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
    #{count => Count, with => lists:reverse(With), without => Without, conf_no => ConfNo}.

% @doc Returns the global text number for the given local text number in the
% given conference on the connected server, or the atom `undefined' if the text
% no longer exists.
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

% @doc Returns the name of the a given conference (or person) from the connected
% server. Caches the name locally for five minutes.
name(Pid, ConfNo) ->
    Cache = elyskom_socket:get_name_cache(Pid),
    Now = erlang:monotonic_time(),
    case ets:lookup(Cache, ConfNo) of
        [{ConfNo, Name, Time}] when Now - Time < 300_000_000_000 ->
            Name;
        [{_ConfNo, _Name, _Time}] ->
            get_and_store_name(Pid, Cache, ConfNo);
        [] ->
            get_and_store_name(Pid, Cache, ConfNo)
    end.

%%%
%%% Internal functions
%%%

get_and_store_name(Pid, Cache, ConfNo) ->
    case elyskom:get_uconf_stat(Pid, ConfNo) of
        {ok, #{name := Name}} ->
            ets:insert(Cache, {ConfNo, Name, erlang:monotonic_time()}),
            Name;
        {error, _, _} ->
            No = erlang:integer_to_binary(ConfNo),
            <<"no such conference #", No/binary>>
    end.

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
