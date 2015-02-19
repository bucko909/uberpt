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
		parse_transform/2
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
					{ok, {ReplacementParams, ReplacementBody}} ->
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

reline(Line, Tree) ->
	ast_apply(Tree,
		fun
			({raw, X}) -> [{raw, X}];
			(T) when is_tuple(T), is_integer(element(2,T)) -> [setelement(2,T,Line)]
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

strip_fragments([{attribute, _, ast_fragment, []}, {function, _, Name, _Arity, [{clause, _, ParamVars, _Guard=[], Body}]} | Rest], ASTAcc, FragAcc) ->
	Params = dict:from_list([ {ParamName, N} || {{var, _, ParamName}, N} <- lists:zip(ParamVars,lists:seq(1,length(ParamVars))) ]),
	strip_fragments(Rest, ASTAcc, [{Name, {Params, Body}}|FragAcc]);
strip_fragments([Head|Rest], ASTAcc, FragAcc) ->
	strip_fragments(Rest, [Head|ASTAcc], FragAcc);
strip_fragments([], ASTAcc, FragAcc) ->
	{lists:reverse(ASTAcc), dict:from_list(FragAcc)}.

ast_apply([String={string, _, _}|Rest], EditFun) ->
	EditFun(String) ++ ast_apply(Rest, EditFun);
ast_apply([String={bin_element, _, _, _, _}|Rest], EditFun) ->
	[ {bin_element, Line, ast_apply(Body, EditFun), ast_apply(N, EditFun), Opt} || {bin_element, Line, Body, N, Opt} <- EditFun(String) ] ++ ast_apply(Rest, EditFun);
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
ast_apply(Thing, EditFun) when tuple_size(Thing) > 2; element(1, Thing) =:= nil; element(1, Thing) =:= eof ->
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
ast_apply_children(Element, EditFun) ->
	[Type,Line|Parts] = tuple_to_list(Element),
	list_to_tuple([Type,Line|[ast_apply(Part, EditFun)||Part<-Parts]]).
