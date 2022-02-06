-module(elyskom_util).

-export([text/2]).
-export([unread/2]).

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
    List = lists:map(
        fun(C) ->
            {ok, M} = elyskom:query_read_texts(Pid, UserId, C, true, 0),
            {ok, UC} = elyskom:get_uconf_stat(Pid, C),
            unread_list(Pid, C, M, UC)
        end,
        ConfList
    ),
    List.

unread_list(Pid, ConfNo, Membership, #{highest_local_no := HighestLocal}) ->
    LastRead = last_text_read(Membership),
    case LastRead < HighestLocal of
        false ->
            [];
        true ->
            {ok, Res0} = elyskom:local_to_global(Pid, ConfNo, LastRead, 255),
            Res = maybe_extend(Pid, ConfNo, Res0),
            {ConfNo, Res}
    end.

last_text_read(#{read_ranges := []}) ->
    1;
last_text_read(#{read_ranges := Ranges}) ->
    lists:foldl(
        fun({_, H}, Acc) ->
            case H > Acc of
                true -> H;
                false -> Acc
            end
        end,
        1,
        Ranges
    ).

maybe_extend(_Pid, _ConfNo, #{more_texts_exist := false} = Res) ->
    Res;
maybe_extend(_Pid, _ConfNo, #{block := Block} = Res) when length(Block) >= 1000 ->
    Res;
maybe_extend(Pid, ConfNo, #{
    more_texts_exist := true,
    range_begin := Begin,
    range_end := End,
    block := Block
}) ->
    {ok, #{block := NewBlock} = NewRes} =
        elyskom:local_to_global(Pid, ConfNo, End, 255),
    maybe_extend(
        Pid,
        ConfNo,
        NewRes#{block := Block ++ NewBlock, range_begin := Begin}
    ).
