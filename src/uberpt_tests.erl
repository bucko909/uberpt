-module(uberpt_tests).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, uberpt}).
-compile(export_all).

-ast_fragment([]).
ast_fragment_basic() ->
	1.

-ast_fragment([]).
ast_fragment_params(A, B, C) ->
	{X, Y} = {A, B},
	{C, X, Y, B}.

-ast_fragment2([]).
ast_fragment2_params(
		{in, [Foo]},
		{out, [Bar]},
		{temp, []}
	) ->
	Bar = Foo + 1.

test4() ->
	A = ast(1),
	ast(quote(A) + 1).

test5(Ast) ->
	A = ast(begin x(), y() end),
	ast_function(foo, fun (A) -> quote(A); ({B}) -> quote(Ast) end).

-ast_forms_function(#{name => ast_forms_stuff, params => ['A', 'B', 'C']}).
a() -> ok.
-test(aaa).
a(B) -> A.
-end_ast_forms_function([]).

all_test_() ->
	[
		% Check ast/1 works as advertised.
		?_assertMatch({atom, ?LINE, a}, ast(a)),
		?_assertMatch({op, ?LINE, '+', {integer, ?LINE, 1}, {integer, ?LINE, 2}}, ast(1 + 2)),
		% Check quoted stuff is passed through exactly.
		?_assertMatch(a, ast(quote(a))),
		% Check ast_fragment produces a list of expressions.
		?_assertMatch([{integer, _, 1}], ast_fragment_basic()),
		% More complex test. This one passes in some naughty non-AST params
		% to an AST-returning function, but the function shouldn't really
		% care anyway. We just do this to make the match more readable.
		?_assertMatch(
			[
				{match, _, {tuple, _, [{var, _, 'X'}, {var, _, 'Y'}]}, {tuple, _, [tag1, tag2]}},
				{tuple, _, [tag3, {var, _, 'X'}, {var, _, 'Y'}, tag2]}
			],
			ast_fragment_params(tag1, tag2, tag3)),
		?_assertMatch(
			[
				{match, _, {var, _, 'TestOutVar'}, {op, _, '+', {var, _, 'TestInVar'}, {integer, _, 1}}}
			]
			,
			ast_fragment2_params({in, [ast(TestInVar)]}, {out, [ast(TestOutVar)]}, {temp_suffix, "NotNeeded"})),
		?_assertMatch(
			[
				{function, _, 'a', 0, [{clause, _, [], [], [{atom, _, ok}]}]},
				{attribute, _, test, aaa},
				{function, _, 'a', 1, [{clause, _, [{atom, _, an_atom}], [], [{var, _, 'Test'}]}]}
			],
			ast_forms_stuff(ast(Test), ast(an_atom), i_am_ignored)
		)
	].
