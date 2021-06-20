-module(prot_a_misc_info).

-include("elyskom.hrl").

-export([parse/1]).
-export([to_int/1]).
-export([encode/1]).

-type t() ::
    {recpt, pos_integer()}
    | {cc_recpt, pos_integer()}
    | {bcc_recpt, pos_integer()}
    | {comm_to, pos_integer()}
    | {comm_in, pos_integer()}
    | {footn_to, pos_integer()}
    | {footn_in, pos_integer()}
    | {loc_no, pos_integer()}
    | {rec_time, prot_a_time:t()}
    | {sent_by, pos_integer()}
    | {sent_at, prot_a_time:t()}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
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
    {Time, Rest} = prot_a_time:parse(Tail),
    {{sent_at, Time}, Rest};
parse([<<"15">>, Integer | Tail]) ->
    {{bcc_recpt, ?b2i(Integer)}, Tail}.

-spec encode(t()) -> iodata().
encode({Type, Payload}) ->
    EncodedType = to_int(Type),
    EncodedPayload = prot_a_integer:encode(Payload),
    <<EncodedType/binary, " ", EncodedPayload/binary>>.

to_int(recpt) -> <<"0">>;
to_int(cc_recpt) -> <<"1">>;
to_int(comm_to) -> <<"2">>;
to_int(comm_in) -> <<"3">>;
to_int(footn_to) -> <<"4">>;
to_int(footn_in) -> <<"5">>;
to_int(loc_no) -> <<"6">>;
to_int(rec_time) -> <<"7">>;
to_int(sent_by) -> <<"8">>;
to_int(sent_at) -> <<"9">>;
to_int(bcc_recpt) -> <<"15">>.
