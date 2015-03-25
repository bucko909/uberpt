-module(uberpt_test).

-compile({parse_transform, uberpt}).
-compile(export_all).

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

-ast_fragment2([]).
test3_(
		{in, [Foo]},
		{out, [Bar]},
		{temp, []}
	) ->
	Bar = Foo + 1.
