-module(prot_a_text_mapping).

-export([parse/1]).

-type t() ::
    {pos_integer(), pos_integer(), boolean(), {sparse, [prot_a_pair:t()]}}
    | {pos_integer(), pos_integer(), boolean(), {dense, {pos_integer(), [pos_integer()]}}}.
-export_type([t/0]).

-spec parse([binary()]) -> {t(), [binary()]}.
parse([RangeBegin, RangeEnd, MoreTextsExist, <<"0">> | SparseBlock]) ->
    {PairList, Rest} = prot_a_args:get([[prot_a_pair]], SparseBlock),
    {{RangeBegin, RangeEnd, MoreTextsExist, {sparse, PairList}}, Rest};
parse([RangeBegin, RangeEnd, MoreTextsExist, <<"1">> | DenseBlock]) ->
    {TextList, Rest} = prot_a_args:get([prot_a_integer, [prot_a_integer]], DenseBlock),
    {{RangeBegin, RangeEnd, MoreTextsExist, {dense, TextList}}, Rest}.
