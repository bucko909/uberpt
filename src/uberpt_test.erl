-module(uberpt_test).

-compile({parse_transform, uberpt}).

-export([
		test/1
	]).

-ast_fragment([]).
test1() ->
	1.

-ast_fragment([]).
test2(A, B, C) ->
	{X, Y} = {A, B},
	{C, X, Y, B}.

test(A) ->
	{
		test1(),
		test2(A, revert(2), revert(3))
	}.
