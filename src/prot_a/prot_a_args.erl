-module(prot_a_args).

-export([parse/2]).

parse(Types, ArgList) ->
    {[], Arguments} = lists:foldl(fun(Type, {ArgL, ResL}) ->
        {Result, Tail} = parse_type(Type,ArgL),
        {Tail, [Result | ResL]}
    end, {ArgList, []}, Types),
    lists:reverse(Arguments).

parse_type([_Type], _ArgL) ->
    throw("Cannot deal with arrays yet");
parse_type(Type,ArgL) when is_atom(Type) ->
    Type:parse(ArgL).