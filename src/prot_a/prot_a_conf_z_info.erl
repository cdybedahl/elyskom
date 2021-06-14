-module(prot_a_conf_z_info).

-export([parse/1]).

parse(List) ->
    {
        [Name, RawType, ConfNo],
        Rest
    } = prot_a_args:get([prot_a_string, prot_a_bitstring, prot_a_integer], List),
    Type = prot_a_bitstring:annotate([rd_prot, original, secret, letterbox], RawType),
    {
        #{
            name => Name,
            type => Type,
            conf_no => ConfNo
        },
        Rest
    }.
