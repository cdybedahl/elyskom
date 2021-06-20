-module(prot_a_dynamic_session_info).

-export([parse/1]).

-type t() :: #{
    session_no => pos_integer(),
    person => pos_integer(),
    working_conference => pos_integer(),
    idle_time => pos_integer(),
    flags => prot_a_session_flags:t(),
    what_am_i_doing => unicode:unicode_binary()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {[Session, Person, Conference, IdleTime, Flags, What], Rest} = prot_a_args:get(
        [prot_a_integer, prot_a_integer, prot_a_integer, prot_a_integer, prot_a_session_flags, prot_a_string],
        List
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
