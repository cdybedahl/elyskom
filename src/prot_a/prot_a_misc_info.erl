-module(prot_a_misc_info).

-include("elyskom.hrl").

-export([parse/1]).

parse([<<"0">>, Integer | Tail]) ->
    {{recpt, ?b2i(Integer)}, Tail};
parse([<<"1">>, Integer | Tail]) ->
    {{cc_recpt, ?b2i(Integer)}, Tail};
parse([<<"2">>, Integer | Tail]) ->
    {{comm_to, ?b2i(Integer)}, Tail};
parse([<<"3">>, Integer | Tail]) ->
    {{comm_in, ?b2i(Integer)}, Tail};
parse([<<"4">>, Integer | Tail]) ->
    {{footn_to, ?b2i(Integer)}, Tail};
parse([<<"5">>, Integer | Tail]) ->
    {{footn_in, ?b2i(Integer)}, Tail};
parse([<<"6">>, Integer | Tail]) ->
    {{loc_no, ?b2i(Integer)}, Tail};
parse([<<"7">> | Tail]) ->
    {Time, Rest} = prot_a_time:parse(Tail),
    {{rec_time, Time}, Rest};
parse([<<"8">>, Integer | Tail]) ->
    {{sent_by, ?b2i(Integer)}, Tail};
parse([<<"9">> | Tail]) ->
    {Time, Rest} = prot_a_integer:parse(Tail),
    {{sent_at, Time}, Rest};
parse([<<"15">>, Integer | Tail]) ->
    {{bcc_recpt, ?b2i(Integer)}, Tail}.
