-module(prot_a_text_mapping).

-export([parse/1]).

parse([RangeBegin, RangeEnd, MoreTextsExist, <<"0">> | SparseBlock]) ->
    {PairList, Rest} = prot_a_args:get([[prot_a_pair]], SparseBlock),
    {[RangeBegin, RangeEnd, MoreTextsExist, {sparse, PairList}], Rest};
parse([RangeBegin, RangeEnd, MoreTextsExist, <<"1">> | DenseBlock]) ->
    {TextList, Rest} = prot_a_args:get([prot_a_integer, [prot_a_integer]], DenseBlock),
    {[RangeBegin, RangeEnd, MoreTextsExist, {dense, TextList}], Rest}.
