-module(prot_a_async).

-export([parse/1]).

parse([<<"7">>]) ->
    async_saving_database;
parse(List) ->
    logger:error("Unknown async: ~p", [List]),
    async_wtf_is_that.