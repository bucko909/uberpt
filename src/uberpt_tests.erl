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

-ast_fragment2([]).
ast_fragment2_temp_params(
		{in, [Foo]},
		{out, [Bar]},
		{temp, [Baz]}
	) ->
	Baz = Foo,
	Bar = Baz.

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
				{match, _, {var, _, 'BazTempTest'}, {var, _, 'TestInVar'}},
				{match, _, {var, _, 'TestOutVar'}, {var, _, 'BazTempTest'}}
			]
			,
			ast_fragment2_temp_params({in, [ast(TestInVar)]}, {out, [ast(TestOutVar)]}, {temp_suffix, "TempTest"})),
		?_assertMatch(
			[
				{function, _, 'a', 0, [{clause, _, [], [], [{atom, _, ok}]}]},
				{attribute, _, test, aaa},
				{function, _, 'a', 1, [{clause, _, [{atom, _, an_atom}], [], [{var, _, 'Test'}]}]}
			],
			ast_forms_stuff(ast(Test), ast(an_atom), i_am_ignored)
		),
		?_assertMatch(
			{'fun', _, {clauses,
				[{clause, _, [completely_invalid_ast], [], [{atom, _, ok}]}]
			}},
			ast(fun ({'$uberpt_quote', completely_invalid_ast}) -> ok end)
		)
	].

ast_apply_test_() ->
	TestTerms = [
		ast("foooo"),
		ast([$f, $o, $o]),
		ast([0, 3, 4]),
		ast(x),
		ast(1),
		ast(A),
		ast({}),
		ast([]),
		ast([1|2]),
		ast([1, 2]),
		ast(_),
		ast('_'),
		ast(foo()),
		ast(<<>>),
		ast(<<10>>),
		ast(<<1:5/little-unsigned-integer>>),
		ast(<<"foo"/binary>>),
		ast(<<"foo">>),
		ast({[{[<<>>]}]})
	],
	[
		% Check that ast_apply with the list-identity function does nothing.
		?_assertEqual(X, uberpt:ast_apply(X, fun (A) -> [A] end))
		|| X <- TestTerms
	] ++ [
		% Test some ast functions.
		?_assertEqual(ast(X - Y), uberpt:ast_apply(ast(X + Y), fun ({op, Line, '+', X, Y}) -> [{op, Line, '-', X, Y}]; (Other) -> [Other] end)),
		?_assertEqual(ast([<<1, 2, "foo">>]), uberpt:ast_apply(ast([<<1, "foo">>]), fun (Elt={bin_element, Line, {integer, Line, X}, Type, Opts}) -> [Elt, {bin_element, Line, {integer, Line, X + 1}, Type, Opts}]; (Other) -> [Other] end)),
		?_assertEqual(ast(<<6:6/integer>>), uberpt:ast_apply(ast(<<5:5/integer>>), fun ({integer, Line, 5}) -> [{integer, Line, 6}]; (Other) -> [Other] end)),
		?_assertEqual(ast([<<>>]), uberpt:ast_apply(ast([<<1, "foo">>]), fun (Elt={bin_element, _, _, _, _}) -> []; (Other) -> [Other] end))
	].
