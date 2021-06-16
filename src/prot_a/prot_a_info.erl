-module(prot_a_info).

-export([encode/1]).

encode(Atom) -> prot_a_misc_info:to_int(Atom).
