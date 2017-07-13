-module(uberpt).

-export([
		parse_transform/2,
		ast_apply/2
	]).

parse_transform(AST, _Opts) ->
	{PurestAst, Fragments} = strip_fragments(AST, [], []),
	FinalAst = ast_apply(PurestAst,
		fun
			({call, Line, {atom, Line, ast}, [Param]}) ->
				[term_to_ast(Line, Param)];
			({call, Line, {atom, Line, ast_function}, [Name, Param]}) ->
				[term_to_ast(Line, make_function(Name, Param))];
			(Call = {call, Line, {atom, _, Name}, Params}) ->
				case dict:find(Name, Fragments) of
					error ->
						[Call];
					{ok, {type_1, ReplacementParams, ReplacementBody}} ->
						[term_to_ast(Line, reline(Line, ast_apply(ReplacementBody, replace_vars_fun(ReplacementParams, Params))))]
				end;
			(Other) ->
				[Other]
		end),
	FinalAst.

replace_vars_fun(FragmentParams, CallParams) ->
	fun
		(Var={var, _, Name}) ->
			case dict:find(Name, FragmentParams) of
				error ->
					[Var];
				{ok, N} ->
					[{raw, lists:nth(N, CallParams)}]
			end;
		(Other) ->
			[Other]
	end.

quote_vars_fun(VList) ->
	NList = [ begin {var, _, Name} = Var, Name end || Var <- VList ],
	fun
		(Var={var, _, Name}) ->
			case lists:member(Name, NList) of
				true ->
					[{raw, Var}];
				false ->
					[Var]
			end;
		(Other) ->
			[Other]
	end.

reline(Line, Tree) ->
	ast_apply(Tree,
		fun
			({raw, X}) -> [{raw, X}];
			({clauses, L}) -> [{clauses, [reline(Line, X) || X <- L]}];
			(T) when is_tuple(T), is_integer(element(2,T)) -> [setelement(2,T,Line)];
			(L) when is_list(L) -> [[reline(Line, X) || X <- L]]
		end).

term_to_ast(_Line, {raw, X}) ->
	X;
term_to_ast(_Line1, {call, _Line2, {atom, _Line3, quote}, [Param]}) ->
	Param;
term_to_ast(Line, X) when is_tuple(X) ->
	{tuple, Line, [term_to_ast(Line, Y) || Y <- tuple_to_list(X)]};
term_to_ast(Line, [X|Y]) ->
	{cons, Line, term_to_ast(Line, X), term_to_ast(Line, Y)};
term_to_ast(Line, []) ->
	{nil, Line};
term_to_ast(Line, X) when is_integer(X) ->
	{integer, Line, X};
term_to_ast(Line, X) when is_atom(X) ->
	{atom, Line, X}.

deblock_clause(C={clause, _Line, _Match, _Guard, [{call, _Line2, {atom, _Line3, quote_block}, [Param]}]}) ->
	setelement(5, C, {raw, Param});
deblock_clause(C) ->
	C.

make_function(Name, {'fun', Line, {clauses, Clauses=[{clause, _, Match, _Guard, _Body}|_]}}) ->
	Arity = length(Match),
	{'function', Line, Name, Arity, lists:map(fun deblock_clause/1, Clauses)}.

flatten_cons({cons, _, L, R}) ->
	[L | flatten_cons(R)];
flatten_cons({nil, _}) ->
	[].


strip_fragments([{attribute, Line, ast_forms_function, Params}|Rest], ASTAcc, FragAcc) ->
	{Inside, [_EndMarker|AfterEndMarker]} =
		lists:splitwith(
			fun
				({attribute, _, end_ast_forms_function, []}) -> false;
				(_) -> true
			end,
			Rest
		),

	case Params of
		#{name := Name} when is_atom(Name) ->
			ok
	end,
	case Params of
		#{params := RawParams} when is_list(RawParams) ->
			true = lists:all(fun is_atom/1, RawParams),
			FunParams = [{var, Line, ParamName} || ParamName <- RawParams];
		_ ->
			FunParams = []
	end,
	% FunParams just happens to be the right shape already.
	Body = ast_apply(Inside, quote_vars_fun(FunParams)),

	strip_fragments(AfterEndMarker, [{function, Line, Name, length(FunParams), [{clause, Line, FunParams, _Guards=[], [term_to_ast(Line, Body)]}]}|ASTAcc], FragAcc);
strip_fragments([{attribute, _, ast_fragment, []}, {function, FLine, FName, Arity, [{clause, CLine, ParamVars, _Guard=[], Body}]} | Rest], ASTAcc, FragAcc) ->
	% quote the parameters.
	WithQuotedVars = ast_apply(Body, quote_vars_fun(ParamVars)),

	% Then revert the body to an abstract syntax tree.
	NewBody = [term_to_ast(FLine, WithQuotedVars)],
	NewFunDef = {function, FLine, FName, Arity, [{clause, CLine, ParamVars, [], NewBody}]},

	% Inject and continue to next toplevel form.
	strip_fragments(Rest, [NewFunDef|ASTAcc], FragAcc);

strip_fragments([{attribute, _, ast_fragment2, []}, {function, FLine, FName, 3, [{clause, CLine, ParamVars, _Guard=[], Body}]} | Rest], ASTAcc, FragAcc) ->
	{InParams, OutParams, TempParams} = ast_fragment2_extract_param_vars(ParamVars),
	NewParamVars = ast_fragment2_replacement_clause(ParamVars),

	% Replace all known variables in the AST with `quote` calls.
	AllVars = InParams ++ OutParams ++ TempParams,
	WithQuotedVars = ast_apply(Body, quote_vars_fun(AllVars)),

	% Generate our replacement function definition.
	TempVarsInit = lists:map(fun ast_fragment2_create_temp_vars/1, TempParams),
	NewBody = TempVarsInit ++ [term_to_ast(FLine, WithQuotedVars)],
	NewFunDef = {function, FLine, FName, 3, [{clause, CLine, NewParamVars, [], NewBody}]},

	% Inject it and continue to the next toplevel form.
	strip_fragments(Rest, [NewFunDef|ASTAcc], FragAcc);

strip_fragments([Head|Rest], ASTAcc, FragAcc) ->
	strip_fragments(Rest, [Head|ASTAcc], FragAcc);

strip_fragments([], ASTAcc, FragAcc) ->
	{lists:reverse(ASTAcc), dict:from_list(FragAcc)}.


ast_fragment2_extract_param_vars(
	[
		{tuple, _, [{atom, _, in}, InParamsCons]},
		{tuple, _, [{atom, _, out}, OutParamsCons]},
		{tuple, _, [{atom, _, temp}, TempParamsCons]}
	]
) ->
	{flatten_cons(InParamsCons), flatten_cons(OutParamsCons), flatten_cons(TempParamsCons)}.

ast_fragment2_replacement_clause([
		InParamsTuple,
		OutParamsTuple,
		{tuple, TempLine, [{atom, _, temp}, TempParamsCons]}
]) ->
	% Prevent compile warnings where there are no temp variables.
	TempSuffixVarName = case TempParamsCons of
		{nil, _} -> '_TempSuffixVar';
		_ -> 'TempSuffixVar'
	end,
	[
		% In/Out params are passed verbatim.
		InParamsTuple,
		OutParamsTuple,
		% Temp parameter is replaced with a temp_suffix param.
		{tuple, TempLine, [{atom, TempLine, temp_suffix}, {var, TempLine, TempSuffixVarName}]}
	].

ast_fragment2_create_temp_vars({var, ALine, Name}) ->
	% These clauses are prepended to ast_fragment2 functions.
	% The code following them will be abstract code to generate the function
	% body, but will have variables called Name inside -- these need to be
	% replaced by the abstract code for a variable called Name ++ TempSuffixVar.
	%
	% Example:
	% -ast_fragment2([]).
	% foo({in, [A]}, {out, [B]}, {temp, [Name]}) ->
	%   Name = A,
	%   B = Name.
	%
	% Generates a body similar to:
	%
	% foo({in, [A]}, {out, [B]}, {temp_suffix, TempSuffixVar}) ->
	%   % OUR CODE
	%   Name = {var, ?LINE, list_to_atom("Name" ++ TempSuffixVar)},
	%   % END OUR CODE
	%   [
	%     {match, _, Name, A},
	%     {match, _, B, Name}
	%   ].

	NameAsStringAst = term_to_ast(ALine, atom_to_list(Name)),
	FullNameAst = {op, ALine, '++', NameAsStringAst, {var, ALine, 'TempSuffixVar'}},
	ErlangListToAtomAst = {remote, ALine, {atom, ALine, erlang}, {atom, ALine, list_to_atom}},
	FunctionApplicationAst = {call, ALine, ErlangListToAtomAst, [FullNameAst]},
	{match, ALine,
		{var, ALine, Name}, % Name =
		{tuple, ALine,
			[
				{atom, ALine, var},
				{integer, ALine, ALine},
				FunctionApplicationAst
			]
		}
	}.

ast_apply([String={string, _, _}|Rest], EditFun) ->
	EditFun(String) ++ ast_apply(Rest, EditFun);
ast_apply([BinElement={bin_element, _, _, _, _}|Rest], EditFun) ->
	[ {bin_element, Line, ast_apply(Body, EditFun), ast_apply(N, EditFun), Opt} || {bin_element, Line, Body, N, Opt} <- EditFun(BinElement) ] ++ ast_apply(Rest, EditFun);
ast_apply([Head|Rest], EditFun) ->
	NewHead = EditFun(Head),
	case NewHead of
		[Head] ->
			ok; %io:format("EditFun1(~p) did nothing~n", [Head]);
		_ ->
			ok %io:format("EditFun1(~p) -> ~p~n", [Head, NewHead])
	end,
	Bits = [ ast_apply_children(NewHeadBit, EditFun) || NewHeadBit <- NewHead ],
	Bits ++ ast_apply(Rest, EditFun);
ast_apply([], _) ->
	[];
ast_apply(String={string, _, _}, EditFun) ->
	[New] = EditFun(String),
	New;
ast_apply(Thing, EditFun) when tuple_size(Thing) > 2; element(1, Thing) =:= nil; element(1, Thing) =:= eof; element(1, Thing) =:= clauses ->
	[New] = EditFun(Thing),
	case New of
		Thing ->
			ok; %io:format("EditFun2(~p) did nothing~n", [Thing]);
		_ ->
			ok %io:format("EditFun2(~p) -> ~p~n", [Thing, New])
	end,
	ast_apply_children(New, EditFun);
ast_apply(Thing, _EditFun) ->
	Thing.

ast_apply_children(Elements, EditFun) when is_list(Elements) ->
	[ ast_apply_children(Element, EditFun) || Element <- Elements ];
ast_apply_children({clauses, Clauses}, EditFun) ->
	% Why?
	{clauses, [ ast_apply(Clause, EditFun) || Clause <- Clauses ]};
ast_apply_children(Element, EditFun) ->
	[Type,Line|Parts] = tuple_to_list(Element),
	list_to_tuple([Type,Line|[ast_apply(Part, EditFun)||Part<-Parts]]).
