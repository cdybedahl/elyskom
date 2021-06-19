-module(prot_a_args).

-export([parse/2]).
-export([get/2]).

-spec parse([atom() | list(atom())], [binary()]) -> [any()].
parse(Types, ArgList) ->
    {Arguments, []} = get(Types, ArgList),
    Arguments.

-spec get([atom() | list(atom())], [binary()]) -> {[any()], [binary()]}.
get(Types, ArgList) ->
    {Tail, Arguments} = lists:foldl(
        fun(Type, {ArgL, ResL}) ->
            {Result, Tail} = parse_type(Type, ArgL),
            {Tail, [Result | ResL]}
        end,
        {ArgList, []},
        Types
    ),
    {lists:reverse(Arguments), Tail}.

-spec parse_type([atom()] | atom(), [binary()]) -> any().
parse_type([Type], ArgL) ->
    prot_a_array:parse(ArgL, Type);
parse_type(Type, ArgL) when is_atom(Type) ->
    Type:parse(ArgL).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_test() ->
    Res = get(
        [prot_a_integer, prot_a_integer, prot_a_string],
        [<<"1">>, <<"2">>, <<"Foo">>, <<"Bar">>]
    ),
    ?assertEqual({[1, 2, <<"Foo">>], [<<"Bar">>]}, Res).

-endif.
