-module(prot_a_dynamic_session_info).

-export([parse/1]).

parse(List) ->
    {[Session, Person, Conference, IdleTime, RawFlags, What], Rest} = prot_a_args:get(
        [prot_a_integer, prot_a_integer, prot_a_integer, prot_a_integer, prot_a_bitstring, prot_a_string],
        List
    ),
    Flags = prot_a_bitstring:annotate(
        [invisible, user_active_used, user_absent, reserved3, reserved4, reserved5, reserved6, reserved7],
        RawFlags
    ),
    {
        #{
            session_no => Session,
            person => Person,
            working_conference => Conference,
            idle_time => IdleTime,
            flags => Flags,
            what_am_i_doing => What
        },
        Rest
    }.
