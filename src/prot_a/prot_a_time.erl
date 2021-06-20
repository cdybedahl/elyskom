-module(prot_a_time).

-include("elyskom.hrl").

-export([encode/1]).
-export([parse/1]).

-type t() :: calendar:datetime().
-export_type([t/0]).

-spec encode(t()) -> iodata().
encode({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    [
        ?i2b(Second),
        ?i2b(Minute),
        ?i2b(Hour),
        ?i2b(Day),
        ?i2b(Month),
        ?i2b(Year),
        <<"0">>,
        <<"0">>,
        %% TODO: This is a lie
        <<"1">>
    ].

-spec parse([binary()]) -> {t(), [binary()]}.
parse(List) ->
    {Args, Tail} = lists:split(9, List),
    [Sec, Min, Hour, Day, Mon, Year, _DoW, _DoY, _DST] =
        prot_a_args:parse(
            [
                %% Seconds
                prot_a_integer,
                %% Minutes
                prot_a_integer,
                %% Hours
                prot_a_integer,
                %% Days
                prot_a_integer,
                %% Months
                prot_a_integer,
                %% Years
                prot_a_integer,
                %% Day of Week
                prot_a_integer,
                %% Day of Year
                prot_a_integer,
                %% IsDST
                prot_a_bool
            ],
            Args
        ),
    {{{1900 + Year, 1 + Mon, Day}, {Hour, Min, Sec}}, Tail}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Incoming = [
        <<"45">>,
        <<"26">>,
        <<"9">>,
        <<"5">>,
        <<"5">>,
        <<"121">>,
        <<"6">>,
        <<"155">>,
        <<"1">>,
        <<"Extra">>
    ],
    Res = parse(Incoming),
    ?assertEqual({{{2021, 6, 5}, {9, 26, 45}}, [<<"Extra">>]}, Res).

-endif.
