-module(prot_a_error).

-export([to_atom/1]).

-type t() :: {error, atom(), non_neg_integer()}.
-export_type([t/0]).

to_atom(0) -> no_error;
to_atom(2) -> not_implemented;
to_atom(3) -> obsolete_call;
to_atom(4) -> invalid_password;
to_atom(5) -> string_too_long;
to_atom(6) -> login_first;
to_atom(7) -> login_disallowed;
to_atom(8) -> conference_zero;
to_atom(9) -> undefined_conference;
to_atom(10) -> undefined_person;
to_atom(11) -> access_denied;
to_atom(12) -> permission_denied;
to_atom(13) -> not_member;
to_atom(14) -> no_such_text;
to_atom(15) -> text_zero;
to_atom(16) -> no_such_local_text;
to_atom(17) -> local_text_zero;
to_atom(18) -> bad_name;
to_atom(19) -> index_out_of_range;
to_atom(20) -> conference_exists;
to_atom(21) -> person_exists;
to_atom(22) -> secret_public;
to_atom(23) -> letterbox;
to_atom(24) -> ldb_error;
to_atom(25) -> illegal_misc;
to_atom(26) -> illegal_info_type;
to_atom(27) -> already_recipient;
to_atom(28) -> already_comment;
to_atom(29) -> already_footnote;
to_atom(30) -> not_recipient;
to_atom(31) -> not_comment;
to_atom(32) -> not_footnote;
to_atom(33) -> recipient_limit;
to_atom(34) -> comment_limit;
to_atom(35) -> footnote_limit;
to_atom(36) -> mark_limit;
to_atom(37) -> not_author;
to_atom(38) -> no_connect;
to_atom(39) -> out_of_memory;
to_atom(40) -> server_is_crazy;
to_atom(41) -> client_is_crazy;
to_atom(42) -> undefined_session;
to_atom(43) -> regexp_error;
to_atom(44) -> not_marked;
to_atom(45) -> temporary_failure;
to_atom(46) -> long_array;
to_atom(47) -> anonymous_rejected;
to_atom(48) -> illegal_aux_item;
to_atom(49) -> aux_item_permission;
to_atom(50) -> unknown_async;
to_atom(51) -> internal_error;
to_atom(52) -> feature_disabled;
to_atom(53) -> message_not_sent;
to_atom(54) -> invalid_membership_type;
to_atom(55) -> invalid_range;
to_atom(56) -> invalid_range_list;
to_atom(57) -> undefined_measurement;
to_atom(58) -> priority_denied;
to_atom(59) -> weight_denied;
to_atom(60) -> weight_zero;
to_atom(61) -> bad_bool.
