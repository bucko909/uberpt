# uberpt

Erlang support library for writing parse transforms.

Use this *as* a parse transform.

Examples below frequently use atoms in place of valid syntax trees to simplify the documentation; in your own code you might pass `quote(ConstantHere)` or some other value which resolves to a valid abstract expression.

## `ast`

Calls to `ast/1` will be replaced with the abstract syntax tree of their parameter. This can be used as shorthand for declaring variables and so-on.

Example: `ast(1)` will become `{integer, _Line, 1}`.

## `ast_function`

Calls to `ast_function/2` will be replaced with the abstract syntax tree of a function whose name is the first parameter and whose body is the body of the fun in the second parameter. This is great for injecting new functions.

`ast_function(foo, fun (1) -> 1; (2) -> 2 end)` will be replaced with:

    {function,_Line,
              {atom,_Line,foo},
              1,
              [{clause,_Line,[{integer,_Line,1}],[],[{integer,_Line,1}]},
               {clause,_Line,[{integer,_Line,2}],[],[{integer,_Line,2}]}]}

## `quote`

Calls to `quote/1` inside an `ast` block will be replaced by the *verbatim* contents of their parameter. This effectively cancels the `ast` wrapper and allows parametrization of the contents..

Example: `ast(quote(A) + 1)` will become `{op,_Line,'+',A,{integer,_Line,1}}`.

## `quote_block`

If a function body in an `ast` block is just a call to `quote_block/1`, the body will become the parameter. This allows the replacement of the body with a list of statements without wrapping them in a `begin`/`end` block.

Example:

    test1(Ast) -> ast_function(foo, fun (1) -> quote(Ast); (2) -> quote_block(Ast) end).

`test7([statement1, statement2])` will be replaced with:

    {function,_Line,
              {atom,_Line,foo},
              1,
              [{clause,_Line,[{integer,_Line,1}],[],[[statement1,statement2]]},
               {clause,_Line,[{integer,_Line,2}],[],[statement1,statement2]}]}

Note the first clause is an invalid syntax tree.

## `'$uberpt_quote'`

Newer erlangs report syntax errors if you write a function call in a match clause. As an alternative, `quote(X)` can be replaced with `{'$uberpt_quote', X}`. For example, `ast(fun ({'$uberpt_quote', X}) -> ok end)` will insert the abstract code `X` as the first match in the function clause, being replaced with:

    {'fun', _Line, {clauses, [{clause, _Line, [X], [], [{atom, _Line, ok}]}]}}

## `ast_fragment`

Preceed a function declaration with `-ast_fragment([]).` to cause the function to return its own abstract syntax tree. Note that this is a *list* of syntax elements.

    -compile({parse_transform, uberpt}).
    -ast_fragment([]).
    test() -> 1.

`test()` returns `[{integer, _Line, 1}]`.

You can have (simple) parameters, which will be replaced into the abstract syntax tree (hence should be valid ast elements!):

    -compile({parse_transform, uberpt}).
    -ast_fragment([]).
    test1(A) -> A.
    -ast_fragment([]).
    test2(A) -> B.
    -ast_fragment([]).
    test3(A) -> A + B.

`test1(1)` returns `[1]`. `test2(1)` returns `[{var, _Line, 'B'}]`. `test3(1)` returns `[{op, _Line, '+', 1, {var, _Line, 'B'}}]`. Note that only the second of these is a valid abstract syntax tree.

## `ast_fragment2`

A different form for `ast_fragment`. You may specify "in params" and "out params" (which are currently equivalent), and a list of "temp params". Calls must pass a `temp_suffix` third argument which can be used to parametrize the variable names which are neither in nor out.

    -compile({parse_transform, uberpt}).
    -ast_fragment2([]).
    test({in, [Foo]}, {out, [Bar]}, {temp, [Baz]}) -> Bar = Foo + Baz.

`test({in, [param1]}, {out, [param2]}, {temp_suffix, "blah"})` returns `[{match, _Line, param2, {op, _Line, '+', param1, {var, _Line, 'Bazblah'}}}]`.

## `ast_forms_function`

Generate a function which returns the complete AST for all forms between this and the next `-end_ast_forms_function([])`.

    -compile({parse_transform, uberpt}).
    -ast_forms_function(#{name => ast_forms_stuff, params => ['Param1']}).
    a() -> Param1.
    -end_ast_forms_function([]).

`ast_forms_stuff(ast(1))` returns the form for a function called `a` which returns the integer `1`. `params` is optional; if not provided, a 0-arity function will be generated.

Note that any unbound variables in the form currently won't generate compile errors until the generated form is itself compiled (that is to say, dropping `, params => ['Param1']` above would not create a compile warning or error).

Some attributes are handled by the preprocessor and either can't be parse-transformed at all or have some verification on their parameters. Any attribute whose name begins `uberpt_raw_` will have that prefix removed, allowing a means of generating these in a semi-natural way.

Additionally, if you want variables in attributes, they must be manually quoted, as the terms in attributes are passed through verbatim and can't contain free variables. Example:

    -compile({parse_transform, uberpt}).
    -ast_forms_function(#{name => ast_attribute_example, params => ['Param1']}).
    -uberpt_raw_module({raw, {var, 1, 'Param1'}}).
    -end_ast_forms_function([]).

Then `ast_attribute_example(foo)` will produce `-module(foo).`.
