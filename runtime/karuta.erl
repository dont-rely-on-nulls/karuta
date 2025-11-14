-module(karuta).

-export([environment_behavior/1, test/0]).

test() ->
  Env = spawn(karuta, environment_behavior, [#{}]),
  Env ! {fresh, self()},
  {fresh_variable, A} = fun () -> receive V -> V end end(),
  Env ! {unify, A, [1, 2, 3], self()},
  fun () -> receive V -> V end end(),
  Env ! {deref, A, self()},
  fun () -> receive V -> V end end().

environment_behavior(State) ->
  environment_behavior(
    receive
      {unify, LHS, RHS, Pid} ->
        maybe
          {ok, NewState} ?= unify(State, LHS, RHS),
          Pid ! {ok, variable_bound},
          NewState
        else
          Error ->
            Pid ! {error, Error},
            State
        end;
      {fresh, Pid} ->
        Var = make_ref(),
        Pid ! {fresh_variable, Var},
        State#{Var => unbound};
      {deref, Var, Pid} ->
        Pid ! {dereferenced, deref(State, Var)},
        State;
      M -> io:format("Unknown message: ~p\n", [M]), State
    end).

deref(State, Var) when is_reference(Var) ->
  case State of
    #{Var := {bound, Value}} -> deref(State, Value);
    _ -> Var
  end;
deref(_, Value) ->
  Value.

unify(State, A, B) ->
  unify_dereferenced(State, deref(State, A), deref(State, B)).

unify_variable(State, Var, Value) ->
  {ok, State#{Var => {bound, Value}}}.

unify_tuple(State, Size, Position, Left, Right) when Position =< Size ->
  maybe
    {ok, NextState} ?= unify(State, element(Position, Left), element(Position, Right)),
    unify_tuple(NextState, Size, Position + 1, Left, Right)
  end;
unify_tuple(State, _, _, _, _) ->
  {ok, State}.

unify_kv(Key, Value, {ok, State}, Map) when map_get(Key, Map) ->
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
unify_dereferenced(State, A, B) when is_reference(A) andalso map_get(A, State) ->
  unify_variable(State, A, B);
unify_dereferenced(State, A, B) when is_reference(B) andalso map_get(B, State) ->
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
