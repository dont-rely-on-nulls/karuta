-module(karuta).

-export([fresh/1, unify/3, discard/1, deref/2, is_variable/2,
         call_with_fresh/1, eq/2, conj/1, disj/1, conj/2, disj/2, delay/1,
         pull/1, start/1, true/1, false/1, take_all/1, deref_query_var/2,
         deref_query/1, query_variable/3, merge_results/3]).

%% TODO: Swap these with a series of unit tests
%% test_pop() ->
%%   receive V ->
%%     io:format("Received: ~p\n", [V]),
%%     V
%%   end.

%% test() ->
%%   Env = make_environment(),
%%   Env ! {fresh, self()},
%%   {fresh_variable, A} = test_pop(),
%%   Env ! {new_choice, self()},
%%   test_pop(),
%%   Env ! {discard, self()},
%%   {discard, Wildcard} = test_pop(),
%%   Env ! {unify, [A|Wildcard], [1, 2, 3], self()},
%%   test_pop(),
%%   Env ! {deref, A, self()},
%%   test_pop(),
%%   Env ! {deref, Wildcard, self()},
%%   test_pop(),
%%   Env ! {backtrack, self()},
%%   test_pop(),
%%   Env ! {deref, A, self()},
%%   test_pop().

is_variable(Var, Bindings) ->
  is_reference(Var) andalso is_map_key(Var, Bindings).

fresh(Bindings) ->
    Var = make_ref(),
    {Var, Bindings#{Var => unbound}}.

discard(Bindings) ->
    case Bindings of
        #{discard := Discard} -> {Discard, Bindings};
        _ -> Discard = make_ref(),
             {Discard, Bindings#{discard => Discard, Discard => discard}}
    end.

deref(State, Var) when is_reference(Var) ->
  case State of
    #{Var := {bound, Value}} -> deref(State, Value);
    _ -> Var
  end;
deref(_, Value) ->
  Value.

deref_tuple(State, Position, Tuple) when Position =< tuple_size(Tuple) ->
  deref_tuple(State, Position + 1,
    setelement(Position, Tuple, deref_all(State, element(Position, Tuple)))
  );
deref_tuple(_, _, Tuple) ->
  Tuple.

deref_map(State, Map) ->
  maps:map(fun(_, V) -> deref_all(State, V) end, Map).

deref_all(State, Var) ->
  Val = deref(State, Var),
  case Val of
    [H | T] -> [deref_all(State, H) | deref_all(State, T)];
    T when is_tuple(T) -> deref_tuple(State, 1, T);
    M when is_map(M) -> deref_map(State, M);
    SomethingElse -> SomethingElse
  end.

deref_query_var(State = #{query := Query}, VarName) when is_map_key(VarName, Query) ->
  deref_all(State, map_get(VarName, Query)).

deref_query(State = #{query := Query}) -> deref_all(State, Query).

query_variable(Var, Name, Goal) ->
  fun (State) ->
    Goal(State#{query => (maps:get(query, State, #{}))#{Name => Var}})
  end.

unify(State, LHS, RHS) ->
  unify_dereferenced(State, deref(State, LHS), deref(State, RHS)).

unify_variable(State, Var, Value) ->
  case State of
    #{Var := discard} -> [State];
    #{Value := discard} -> [State];
    _ -> [State#{Var => {bound, Value}}]
  end.

unify_tuple(State, Size, Position, Left, Right) when Position =< Size ->
  maybe
    [NextState] ?= unify(State, element(Position, Left), element(Position, Right)),
    unify_tuple(NextState, Size, Position + 1, Left, Right)
  end;
unify_tuple(State, _, _, _, _) ->
  [State].

unify_kv(Key, Value, [State], Map) when is_map_key(Key, Map) ->
  unify(State, Value, map_get(Key, Map));
unify_kv(_, _, _, _) ->
  [].

unify_map(State, A, B) ->
  maps:fold(
    fun (Key, Value, Acc) -> unify_kv(Key, Value, Acc, B) end,
    [State],
    A
  ).

unify_dereferenced(State, A, B) when
  ((is_atom(A) andalso is_atom(B)) orelse
   (is_bitstring(A) andalso is_bitstring(B)) orelse
   (is_float(A) andalso is_float(B)) orelse
   (is_integer(A) andalso is_integer(B)) orelse
   (is_pid(A) andalso is_pid(B)) orelse
   (is_port(A) andalso is_port(B)) orelse
   (is_reference(A) andalso is_reference(B))) andalso A =:= B ->
  [State];
unify_dereferenced(State, A, B) when is_reference(A) andalso is_map_key(A, State) ->
  unify_variable(State, A, B);
unify_dereferenced(State, A, B) when is_reference(B) andalso is_map_key(B, State) ->
  unify_variable(State, B, A);
unify_dereferenced(State, [HA|TA], [HB|TB]) ->
  maybe
    [NextState] ?= unify(State, HA, HB),
    unify(NextState, TA, TB)
  end;
unify_dereferenced(State, [], []) ->
  [State];
unify_dereferenced(State, A, B) when is_map(A) andalso is_map(B) andalso map_size(A) == map_size(B) ->
  unify_map(State, A, B);
unify_dereferenced(State, A, B) when is_tuple(A) andalso is_tuple(B) andalso tuple_size(A) == tuple_size(B) ->
  unify_tuple(State, tuple_size(A), 1, A, B);
unify_dereferenced(_, _, _) ->
  [].

% mzero() -> [].

mplus([], RHS) -> RHS;
mplus([H | T], RHS) -> [H | mplus(T, RHS)];
mplus(LHS, RHS) when is_function(LHS) -> fun() -> mplus(RHS, LHS()) end.

bind([], _) -> [];
bind([H | T], Goal) -> mplus(Goal(H), bind(T, Goal));
bind(Stream, Goal) when is_function(Stream) -> fun() -> bind(Stream(), Goal) end.

delay(Goal) -> fun(State) -> fun() -> Goal(State) end end.

pull([]) -> {error, no_result};
pull([H | T]) -> {ok, H, T};
pull(Stream) when is_function(Stream) -> pull(Stream()).

start(Goal) -> Goal(#{}).

take_all(Stream) ->
  case pull(Stream) of
    {ok, Res, Next} -> [Res | take_all(Next)];
    {error, no_result} -> []
  end.

%% Primitives
eq(LHS, RHS) -> fun(State) -> unify(State, LHS, RHS) end.

true(State) -> [State].
false(_) -> [].

call_with_fresh(F) ->
  fun(State) ->
    {Var, NewState} = fresh(State),
    Goal = F(Var),
    Goal(NewState)
  end.

disj(G1, G2) ->
  fun(State) -> mplus(G1(State), G2(State)) end.

conj(G1, G2) ->
  fun(State) -> bind(G1(State), G2) end.

disj(Goals) ->
  lists:foldr(
    fun(Elem, Acc) -> disj(delay(Elem), Acc) end,
    fun false/1,
    Goals).

conj(Goals) ->
  lists:foldr(
    fun(Elem, Acc) -> conj(delay(Elem), Acc) end,
    fun true/1,
    Goals).

merge_results(Stream, Pattern, Results) ->
  fun() ->
    case pull(Results) of
      {ok, Head, Tail} ->
        mplus(
          bind(Stream, eq(Pattern, Head)),
          merge_results(Stream, Pattern, Tail)
        );
      {error, no_result} -> []
    end
  end.
