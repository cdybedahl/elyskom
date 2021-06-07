-module(prot_a_args).

-include("elyskom.hrl").

-export([parse/2]).
-export([get/2]).

parse(Types, ArgList) ->
    {Arguments, []} = get(Types, ArgList),
    Arguments.

get(Types, ArgList) ->
    {Tail, Arguments} = lists:foldl(fun(Type, {ArgL, ResL}) ->
        {Result, Tail} = parse_type(Type,ArgL),
        {Tail, [Result | ResL]}
    end, {ArgList, []}, Types),
    {lists:reverse(Arguments), Tail}.

parse_type([Type], ArgL) ->
    prot_a_array:parse(ArgL, Type);
parse_type(Type,ArgL) when is_atom(Type) ->
    Type:parse(ArgL).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_test() ->
    Res = get(
        [prot_a_integer, prot_a_integer, prot_a_string],
        [<<"1">>, <<"2">>, <<"Foo">>, <<"Bar">>]
    ),
    ?assertEqual({[1,2,<<"Foo">>], [<<"Bar">>]}, Res).

-endif.