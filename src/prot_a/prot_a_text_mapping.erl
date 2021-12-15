-module(prot_a_text_mapping).

-export([parse/1]).

-include("elyskom.hrl").

-type block() :: {sparse, [prot_a_pair:t()]} | {dense, {pos_integer(), [pos_integer()]}}.
-type t() ::
    #{range_begin => pos_integer(), range_end => pos_integer(), more_texts_exist => boolean(), block => block()}.
-export_type([t/0, block/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse([RangeBegin, RangeEnd, MoreTextsExist, <<"0">> | SparseBlock]) ->
    {PairList, Rest} = prot_a_array:parse(SparseBlock, prot_a_pair),
    {
        #{
            range_begin => ?b2i(RangeBegin),
            range_end => ?b2i(RangeEnd),
            more_texts_exist => MoreTextsExist == <<"1">>,
            block => {sparse, PairList}
        },
        Rest
    };
parse([RangeBegin, RangeEnd, MoreTextsExist, <<"1">> | DenseBlock]) ->
    {TextList, Rest} = prot_a_args:get([prot_a_integer, [prot_a_integer]], DenseBlock),
    {
        #{
            range_begin => ?b2i(RangeBegin),
            range_end => ?b2i(RangeEnd),
            more_texts_exist => MoreTextsExist == <<"1">>,
            block => {dense, TextList}
        },
        Rest
    }.
