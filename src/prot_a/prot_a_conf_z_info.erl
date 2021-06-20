-module(prot_a_conf_z_info).

-export([parse/1]).

-type t() :: #{
    name => prot_a_string:t(),
    type => prot_a_extended_conf:any_conf(),
    conf_no => pos_integer()
}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {
        [Name, Type, ConfNo],
        Rest
    } = prot_a_args:get([prot_a_string, prot_a_extended_conf, prot_a_integer], List),
    {
        #{
            name => Name,
            type => Type,
            conf_no => ConfNo
        },
        Rest
    }.
