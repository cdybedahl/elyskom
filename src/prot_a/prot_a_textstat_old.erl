-module(prot_a_textstat_old).

-export([parse/1]).

parse(List) ->
    {CreationTime, Rest0} = prot_a_time:parse(List),
    {Author, Rest1} = prot_a_integer:parse(Rest0),
    {NoOfLines, Rest2} = prot_a_integer:parse(Rest1),
    {NoOfChars, Rest3} = prot_a_integer:parse(Rest2),
    {NoOfMarks, Rest4} = prot_a_integer:parse(Rest3),
    {MiscInfo, Rest5} = prot_a_array:parse(Rest4, prot_a_misc_info),
    {#{
        creation_time => CreationTime,
        author => Author,
        no_of_lines => NoOfLines,
        no_of_chars => NoOfChars,
        no_of_marks => NoOfMarks,
        misc_info => MiscInfo
    }, Rest5}.