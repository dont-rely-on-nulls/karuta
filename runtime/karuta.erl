-module(karuta).

-export([fresh/1, unify/3, discard/1, deref/2, is_variable/2]).

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

is_variable(Var, Bindings) when (is_reference(Var) andalso is_map_key(Var, Bindings)) -> true;
is_variable(_, _) -> false.

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

unify(State, LHS, RHS) ->
  unify_dereferenced(State, deref(State, LHS), deref(State, RHS)).

unify_variable(State, Var, Value) ->
  case State of
    #{Var := discard} -> {ok, State};
    #{Value := discard} -> {ok, State};
    _ -> {ok, State#{Var => {bound, Value}}}
  end.

unify_tuple(State, Size, Position, Left, Right) when Position =< Size ->
  maybe
    {ok, NextState} ?= unify(State, element(Position, Left), element(Position, Right)),
    unify_tuple(NextState, Size, Position + 1, Left, Right)
  end;
unify_tuple(State, _, _, _, _) ->
  {ok, State}.

unify_kv(Key, Value, {ok, State}, Map) when is_map_key(Key, Map) ->
  unify(State, Value, map_get(Key, Map));
unify_kv(_, _, _, _) ->
  {error, unification_failed}.

unify_map(State, A, B) ->
  maps:fold(
    fun (Key, Value, Acc) -> unify_kv(Key, Value, Acc, B) end,
    {ok, State},
    A
  ).

unify_dereferenced(State, A, B) when
  ((is_atom(A) andalso is_atom(B)) orelse
   (is_bitstring(A) andalso is_bitstring(B)) orelse
   (is_float(A) andalso is_float(B)) orelse
   (is_integer(A) andalso is_integer(B)) orelse
   (is_pid(A) andalso is_pid(B)) orelse
   (is_port(A) andalso is_port(B)) orelse
   (is_reference(A) andalso is_reference(B))) andalso A == B ->
  {ok, State};
unify_dereferenced(State, A, B) when is_reference(A) andalso is_map_key(A, State) ->
  unify_variable(State, A, B);
unify_dereferenced(State, A, B) when is_reference(B) andalso is_map_key(B, State) ->
  unify_variable(State, B, A);
unify_dereferenced(State, [HA|TA], [HB|TB]) ->
  maybe
    {ok, NextState} ?= unify(State, HA, HB),
    unify(NextState, TA, TB)
  end;
unify_dereferenced(State, [], []) ->
  {ok, State};
unify_dereferenced(State, A, B) when is_map(A) andalso is_map(B) andalso map_size(A) == map_size(B) ->
  unify_map(State, A, B);
unify_dereferenced(State, A, B) when is_tuple(A) andalso is_tuple(B) andalso tuple_size(A) == tuple_size(B) ->
  unify_tuple(State, tuple_size(A), 1, A, B);
unify_dereferenced(_, _, _) ->
  {error, unification_failed}.
