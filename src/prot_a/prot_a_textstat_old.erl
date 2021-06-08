-module(prot_a_textstat_old).

-include("elyskom.hrl").

-export([parse/1]).

parse(List) ->
    {[CreationTime, Author, NoOfLines, NoOfChars, NoOfMarks, MiscInfo], Rest} =
        prot_a_args:get(
            [
                prot_a_time,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                prot_a_integer,
                [prot_a_misc_info]
            ],
            List
        ),
    {
        #{
            creation_time => CreationTime,
            author => Author,
            no_of_lines => NoOfLines,
            no_of_chars => NoOfChars,
            no_of_marks => NoOfMarks,
            misc_info => MiscInfo
        },
        Rest
    }.
