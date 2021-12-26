-module(prot_a_text_mapping).

-export([parse/1]).

-include("elyskom.hrl").

-type block() :: {sparse, [prot_a_pair:t()]} | {dense, {pos_integer(), [pos_integer()]}}.
-type t() ::
    #{range_begin => pos_integer(), range_end => pos_integer(), more_texts_exist => boolean(), block => block()}.
-export_type([t/0, block/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse([RangeBegin, RangeEnd, MoreTextsExist, BlockType | InBlock]) ->
    {Block, Rest} = parse_block(BlockType, InBlock),
    {
        #{
            range_begin => ?b2i(RangeBegin),
            range_end => ?b2i(RangeEnd),
            more_texts_exist => MoreTextsExist == <<"1">>,
            block => Block
        },
        Rest
    }.

parse_block(<<"0">>, Block) ->
    {PairList, Rest} = prot_a_array:parse(Block, prot_a_pair),
    {PairList, Rest};
parse_block(<<"1">>, Block) ->
    {[BaseLocalNo, GlobalNoList], Rest} = prot_a_args:get([prot_a_integer, [prot_a_integer]], Block),
    {PairList, _} = lists:foldl(
        fun(GlobalNo, {List, Counter}) ->
            case GlobalNo of
                0 -> {List, Counter + 1};
                _ -> {[{BaseLocalNo + Counter, GlobalNo} | List], Counter + 1}
            end
        end,
        {[], 0},
        GlobalNoList
    ),
    {PairList, Rest}.
