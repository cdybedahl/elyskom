-module(prot_a_conference).

-export([parse/1]).

-type t() :: #{
    name => prot_a_string:t(),
    type => prot_a_extended_conf:any_conf(),
    creation_time => prot_a_time:t(),
    last_written => prot_a_time:t(),
    creator => pos_integer(),
    presentation => pos_integer(),
    supervisor => pos_integer(),
    permitted_submitters => pos_integer(),
    super_conf => pos_integer(),
    msg_of_day => pos_integer(),
    nice => pos_integer(),
    keep_commented => pos_integer(),
    no_of_members => pos_integer(),
    first_local_no => pos_integer(),
    no_of_texts => pos_integer(),
    expire => pos_integer(),
    aux_items => [prot_a_aux_items:input()]
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {
        [
            Name,
            Type,
            CreationTime,
            LastWritten,
            Creator,
            Presentation,
            Supervisor,
            PermittedSubmitters,
            SuperConf,
            MsgOfDay,
            Nice,
            KeepCommented,
            NoOfMembers,
            FirstLocalNo,
            NoOfTexts,
            Expire,
            AuxItems
        ],
        Rest
    } = prot_a_args:get(
        [
            prot_a_string,
            prot_a_extended_conf,
            prot_a_time,
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
            [prot_a_aux_item]
        ],
        List
    ),
    {
        #{
            name => Name,
            type => Type,
            creation_time => CreationTime,
            last_written => LastWritten,
            creator => Creator,
            presentation => Presentation,
            supervisor => Supervisor,
            permitted_submitters => PermittedSubmitters,
            super_conf => SuperConf,
            msg_of_day => MsgOfDay,
            nice => Nice,
            keep_commented => KeepCommented,
            no_of_members => NoOfMembers,
            first_local_no => FirstLocalNo,
            no_of_texts => NoOfTexts,
            expire => Expire,
            aux_items => AuxItems
        },
        Rest
    }.
