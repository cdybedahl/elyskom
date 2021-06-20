-module(prot_a_person).

-export([parse/1]).

-type t() :: #{
    username => prot_a_string:t(),
    privileges => prot_a_priv_bits:t(),
    flags => prot_a_personal_flags:t(),
    last_login => prot_a_time:t(),
    user_area => pos_integer(),
    total_time_present => pos_integer(),
    sessions => pos_integer(),
    created_lines => pos_integer(),
    created_bytes => pos_integer(),
    read_texts => pos_integer(),
    no_of_text_fetches => pos_integer(),
    created_persons => pos_integer(),
    created_confs => pos_integer(),
    first_created_local_no => pos_integer(),
    no_of_created_texts => pos_integer(),
    no_of_marks => pos_integer(),
    no_of_confs => pos_integer()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {
        [
            Username,
            Privileges,
            Flags,
            LastLogin,
            UserArea,
            TotalTimePresent,
            Sessions,
            CreatedLines,
            CreatedBytes,
            ReadTexts,
            NoOfTextFetches,
            CreatedPersons,
            CreatedConfs,
            FirstCreatedLocalNo,
            NoOfCreatedTexts,
            NoOfMarks,
            NoOfConfs
        ],
        Rest
    } =
        prot_a_args:get(
            [
                prot_a_string,
                prot_a_priv_bits,
                prot_a_personal_flags,
                prot_a_time,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer
            ],
            List
        ),
    {
        #{
            username => Username,
            privileges => Privileges,
            flags => Flags,
            last_login => LastLogin,
            user_area => UserArea,
            total_time_present => TotalTimePresent,
            sessions => Sessions,
            created_lines => CreatedLines,
            created_bytes => CreatedBytes,
            read_texts => ReadTexts,
            no_of_text_fetches => NoOfTextFetches,
            created_persons => CreatedPersons,
            created_confs => CreatedConfs,
            first_created_local_no => FirstCreatedLocalNo,
            no_of_created_texts => NoOfCreatedTexts,
            no_of_marks => NoOfMarks,
            no_of_confs => NoOfConfs
        },
        Rest
    }.
