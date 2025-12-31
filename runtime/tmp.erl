-module(tmp).

-export([parse_tree/1, pluz/3]).

parse_tree(File) ->
  {ok, Ret} = epp:parse_file(File, []),
  Ret.

% pluz[Out, 0, Out] :- nat[Out].
% pluz[0, Out, Out] :- nat[Out].
% pluz[succ[Prev], X, Out] :- pluz[Prev, succ[X], Out].

% pluz(Arg0, Arg1, Arg2) ->
%   karuta:disj([
%     karuta:conj([
%      karuta:eq(Arg0, Arg2),
%      karuta:eq(Arg1, 0)
%     ]),
%     karuta:conj([
%      karuta:eq(Arg1, Arg2),
%      karuta:eq(Arg0, 0)
%     ]),
%     karuta:call_with_fresh(fun(Prev) ->
%       karuta:conj([
%         karuta:eq(Arg0, {succ, Prev}),
%         pluz(Prev, {succ, Arg1}, Arg2)
%       ])
%     end)
%   ]).

% pluz[0, Out, Out] :- nat[Out].
% pluz[succ[Prev], X, succ[NewOut]] :- pluz[Prev, X, NewOut].

nat(N) ->
  karuta:disj([
    karuta:eq(N, 0),
    karuta:call_with_fresh(fun(Prev) ->
      karuta:conj([
        karuta:eq({succ, Prev}, N),
        nat(Prev)
      ])
    end)
  ]).

pluz(Arg0, Arg1, Arg2) ->
  karuta:disj([
    karuta:conj([
      karuta:eq(Arg0, 0),
      karuta:eq(Arg1, Arg2),
      nat(Arg1)
    ]),
    karuta:call_with_fresh(fun(Prev) ->
      karuta:call_with_fresh(fun(NewOut) ->
        karuta:conj([
          karuta:eq(Arg0, {succ, Prev}),
          karuta:eq(Arg2, {succ, NewOut}),
          pluz(Prev, Arg1, NewOut)
        ])
      end)
    end)
  ]).

% karuta:start(karuta:call_with_fresh(
%   fun(LHS) -> karuta:call_with_fresh(fun(RHS) ->
%      karuta:query_variable(LHS, lhs,
%       karuta:query_variable(RHS, rhs,
%        tmp:pluz(LHS, RHS, {succ, {succ, {succ, {succ, 0}}}})))
%    end) end)).

% lists:map(
%   fun (State) ->
%     #{lhs => karuta:deref_query_var(State, lhs),
%       rhs => karuta:deref_query_var(State, rhs)} end,
%   karuta:take_all(Test)).

% karuta:start(karuta:call_with_fresh(
%   fun(Out) ->
%     karuta:query_variable(Out, out,
%       tmp:pluz({succ, {succ, {succ, {succ, 0}}}}, {succ, {succ, 0}}, Out))
%    end)).

% lists:map(
%   fun (State) ->
%     #{out => karuta:deref_query_var(State, out)} end,
%   karuta:take_all(Addition)).
