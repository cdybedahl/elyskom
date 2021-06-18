-module(prot_a_conference).

-export([parse/1]).

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
            keeo_commented => KeepCommented,
            no_of_members => NoOfMembers,
            first_local_no => FirstLocalNo,
            no_of_texts => NoOfTexts,
            expire => Expire,
            aux_items => AuxItems
        },
        Rest
    }.
