-module(elyskom_util).

-export([text/2]).

text(Pid, TextNo) ->
    #{no_of_chars := Size, aux_item := AuxItems} = TS = elyskom:get_text_stat(Pid, TextNo),
    [#{data := Type}] = lists:filter(fun(#{tag := Tag}) -> Tag == 1 end, AuxItems),
    Length = erlang:byte_size(Type),
    {S, L} = binary:match(Type, <<"charset=">>),
    Encoding = binary:part(Type, S + L, Length - (S + L)),
    Raw = elyskom:get_text(Pid, TextNo, 0, Size),
    Text = iconv:convert(Encoding, <<"utf-8">>, Raw),
    [Subject, Body] = binary:split(Text, <<"\n">>),
    TS#{subject => Subject, body => Body}.
