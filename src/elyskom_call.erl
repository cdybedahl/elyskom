-module(elyskom_call).

-export([make/2]).
-export([response/2]).

make(CallName, Args) ->
    Pattern = make_args(CallName),
    Pairs = [call_no(CallName) | Args],
    Elements = lists:map(
        fun
            ({[Type], Data}) ->
                prot_a_array:encode(Type, Data);
            ({Type, Data}) when is_atom(Type) ->
                Type:encode(Data)
        end,
        lists:zip([prot_a_integer | Pattern], Pairs)
    ),
    lists:join(<<" ">>, Elements).

%%% Protocol A call numbers
call_no(re_z_lookup) -> 74;
call_no(lookup_z_name) -> 76;
call_no(who_is_on_dynamic) -> 83;
call_no(get_text_stat) -> 90;
call_no(get_time) -> 35;
call_no(get_text) -> 25;
call_no(get_uconf_stat) -> 78;
call_no(login) -> 62;
call_no(accept_async) -> 80.

%%% Lists of argument types for calls
make_args(re_z_lookup) -> [prot_a_string, prot_a_bool, prot_a_bool];
make_args(lookup_z_name) -> [prot_a_string, prot_a_bool, prot_a_bool];
make_args(who_is_on_dynamic) -> [prot_a_bool, prot_a_bool, prot_a_integer];
make_args(get_text_stat) -> [prot_a_integer];
make_args(get_time) -> [];
make_args(get_text) -> [prot_a_integer, prot_a_integer, prot_a_integer];
make_args(get_uconf_stat) -> [prot_a_integer];
make_args(login) -> [prot_a_integer, prot_a_string, prot_a_bool];
make_args(accept_async) -> [[prot_a_integer]].

%%% How to turn returned lists from calls into something useful
response(re_z_lookup, Args) -> one_arg([prot_a_conf_z_info], Args);
response(lookup_z_name, Args) -> one_arg([prot_a_conf_z_info], Args);
response(who_is_on_dynamic, Args) -> one_arg([prot_a_dynamic_session_info], Args);
response(get_text_stat, Args) -> one_arg(prot_a_textstat, Args);
response(get_time, Args) -> one_arg(prot_a_time, Args);
response(get_text, Args) -> one_arg(prot_a_string, Args);
response(get_uconf_stat, Args) -> one_arg(prot_a_uconference, Args);
response(_CallName, []) -> ok.

%%% Helpers
one_arg(Type, List) ->
    [Arg] = prot_a_args:parse([Type], List),
    Arg.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accept_async_test() ->
    Res = ?l2b(make(accept_async, lists:seq(2, 10, 2))),
    ?assertEqual(<<"80 5 { 2 4 6 8 10 }">>, Res).

-endif.
