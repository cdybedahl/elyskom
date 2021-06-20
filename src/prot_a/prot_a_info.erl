-module(prot_a_info).

-export([encode/1]).

-type t() ::
    (bcc_recpt
    | cc_recpt
    | comm_in
    | comm_to
    | footn_in
    | footn_to
    | loc_no
    | rec_time
    | recpt
    | sent_at
    | sent_by).
-export_type([t/0]).

-spec encode(t()) -> iodata().
encode(Atom) -> prot_a_misc_info:to_int(Atom).
