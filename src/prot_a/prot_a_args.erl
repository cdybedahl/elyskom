-module(prot_a_args).

-include("elyskom.hrl").

-export([parse/2]).

parse(Types, ArgList) ->
    {[], Arguments} = lists:foldl(fun(Type, {ArgL, ResL}) ->
        {Result, Tail} = parse_type(Type,ArgL),
        {Tail, [Result | ResL]}
    end, {ArgList, []}, Types),
    lists:reverse(Arguments).

parse_type([Type], ArgL) ->
    prot_a_array:parse(ArgL, Type);
parse_type(Type,ArgL) when is_atom(Type) ->
    Type:parse(ArgL).