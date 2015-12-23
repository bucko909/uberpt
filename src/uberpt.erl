-module(uberpt).

% -ast_fragment([]).
% ast_function(Param1, Param2) ->
%   f(Param1, Param2).
%
% real_function(Param1) ->
%   generate_module(ast_function(revert(Param1), revert(73))).
%
% The "call" to ast_function becomes the abstract code for the body of the function.
% Don't do anything clever. It probably won't work!

-export([
		parse_transform/2,
		ast_apply/2
	]).

parse_transform(AST, _Opts) ->
	{PurestAst, Fragments} = strip_fragments(AST, [], []),
	FinalAst = ast_apply(PurestAst,
		fun
			({call, Line, {atom, Line, revert}, [Param]}) ->
				[quote(Line, Param)];
			(Call = {call, Line, {atom, _, Name}, Params}) ->
				case dict:find(Name, Fragments) of
					error ->
						[Call];
					{ok, {type_1, ReplacementParams, ReplacementBody}} ->
						[quote(Line, reline(Line, ast_apply(ReplacementBody, replace_vars_fun(ReplacementParams, Params))))]
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

quote(_Line, {raw, X}) ->
	X;
quote(Line, X) when is_tuple(X) ->
	{tuple, Line, [quote(Line, Y) || Y <- tuple_to_list(X)]};
quote(Line, [X|Y]) ->
	{cons, Line, quote(Line, X), quote(Line, Y)};
quote(Line, []) ->
	{nil, Line};
quote(Line, X) when is_integer(X) ->
	{integer, Line, X};
quote(Line, X) when is_atom(X) ->
	{atom, Line, X}.

flatten_cons({cons, _, L, R}) ->
	[L | flatten_cons(R)];
flatten_cons({nil, _}) ->
	[].

make_cons(Line, [L | R]) ->
	{cons, Line, L, make_cons(Line, R)};
make_cons(Line, []) ->
	{nil, Line}.

strip_fragments([{attribute, _, ast_fragment, []}, {function, _, Name, _Arity, [{clause, _, ParamVars, _Guard=[], Body}]} | Rest], ASTAcc, FragAcc) ->
	Params = dict:from_list([ {ParamName, N} || {{var, _, ParamName}, N} <- lists:zip(ParamVars,lists:seq(1,length(ParamVars))) ]),
	strip_fragments(Rest, ASTAcc, [{Name, {type_1, Params, Body}}|FragAcc]);
strip_fragments([{attribute, ALine, ast_fragment2, []}, {function, FLine, FName, 3, [{clause, CLine, ParamVars, _Guard=[], Body}]} | Rest], ASTAcc, FragAcc) ->
	[
		InParamsTuple = {tuple, _, [{atom, _, in}, InParamsCons]},
		OutParamsTuple = {tuple, _, [{atom, _, out}, OutParamsCons]},
		{tuple, TempLine, [{atom, _, temp}, TempParamsCons]}
	] = ParamVars,
	TempParams = flatten_cons(TempParamsCons),
	NewParamVars = [
		InParamsTuple,
		OutParamsTuple,
		{tuple, TempLine, [{atom, TempLine, temp_suffix}, {var, TempLine, 'TempSuffixVar'}]}
	],
	TempVarsInit = [ {match, ALine, {var, ALine, Name}, {tuple, ALine, [{atom, ALine, var}, {integer, ALine, ALine}, {call, ALine, {remote, ALine, {atom, ALine, erlang}, {atom, ALine, list_to_atom}}, [{op, ALine, '++', quote(ALine, atom_to_list(Name)), {var, ALine, 'TempSuffixVar'}}]}]}} || {var, _, Name} <- TempParams ],
	AllVars = flatten_cons(InParamsCons) ++ flatten_cons(OutParamsCons) ++ TempParams,
	NewBody = TempVarsInit ++ [make_cons(FLine, [ quote(FLine, X) || X <- ast_apply(Body, quote_vars_fun(AllVars)) ])],
	NewFunDef = {function, FLine, FName, 3, [{clause, CLine, NewParamVars, [], NewBody}]},
	strip_fragments(Rest, [NewFunDef|ASTAcc], FragAcc);
strip_fragments([Head|Rest], ASTAcc, FragAcc) ->
	strip_fragments(Rest, [Head|ASTAcc], FragAcc);
strip_fragments([], ASTAcc, FragAcc) ->
	{lists:reverse(ASTAcc), dict:from_list(FragAcc)}.

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
