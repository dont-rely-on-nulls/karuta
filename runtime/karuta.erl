-module(karuta).

-export([fresh/1, variable_behavior/2, unify/3, environment_behavior/1]).

environment_behavior(State) ->
  environment_behavior(
    receive
      {bind, Var, Value, Pid} when is_reference(Var) -> case State of
        #{Var := Behavior} -> Behavior ! {bind, Value, Pid};
        _ -> Pid ! {error, not_variable}
        end,
        State;
      {new_variable, Var, Pid} -> State#{Var := Pid};
      {deref, Var, Pid} when is_reference(Var) -> case State of
        #{Var := Behavior} ->
          Behavior ! {read, self()},
          receive
            {ok, Value} -> self() ! {deref, Value, Pid};
            {error, unbound} -> Pid ! Var
          end;
        _ -> Pid ! Var,
        State
      end;
      {deref, Value, Pid} -> Pid ! Value, State
    end).

variable_behavior(Env, State) ->
  variable_behavior(
    Env,
    case State of
      unbound -> receive
        {read, Pid} -> Pid ! {error, unbound}, State;
        {bind, Value, Pid} -> Pid ! ok, {bound, Value}
      end;
      {bound, Value} -> receive
        {read, Pid} -> Pid ! {ok, Value}, State;
        {bind, OtherValue, Pid} -> Pid ! unify(Env, Value, OtherValue), State
      end
    end).

fresh(Env) ->
  Var = make_ref(),
  Env ! {new_variable, Var, spawn(?MODULE, variable_behavior, [Env, unbound])},
  Var.

deref(Env, Var) -> Env ! {deref, Var, self()}, receive V -> V end.

unify(Env, A, B) ->
  unify_dereferenced(Env, deref(Env, A), deref(Env, B)).

unify_variable(Env, Var, Value) ->
  Env ! {bind, Var, Value, self()},
  receive T -> T end.

unify_tuple(Env, Size, Position, Left, Right) when Position =< Size ->
  maybe
    ok ?= unify(Env, element(Position, Left), element(Position, Right)),
    unify_tuple(Env, Size, Position + 1, Left, Right)
  end;
unify_tuple(_, _, _, _, _) -> ok.

unify_kv(Key, Value, ok, Env, Map) when map_get(Key, Map) ->
  unify(Env, Value, map_get(Key, Map));
unify_kv(_, _, ok, _, _) ->
  {error, unification_failed};
unify_kv(_, _, Error, _, _) ->
  Error.

unify_map(Env, A, B) ->
  maps:fold(fun (Key, Value, Acc) -> unify_kv(Key, Value, Acc, Env, B) end, ok, A).

unify_dereferenced(_, A, B) when
  ((is_atom(A) andalso is_atom(B)) orelse
   (is_bitstring(A) andalso is_bitstring(B)) orelse
   (is_float(A) andalso is_float(B)) orelse
   (is_integer(A) andalso is_integer(B)) orelse
   (is_pid(A) andalso is_pid(B)) orelse
   (is_port(A) andalso is_port(B)) orelse
   (is_reference(A) andalso is_reference(B))) andalso A == B ->
  ok;
unify_dereferenced(Env, A, B) when is_reference(A) ->
  unify_variable(Env, A, B);
unify_dereferenced(Env, A, B) when is_reference(B) ->
  unify_variable(Env, B, A);
unify_dereferenced(Env, [HA|TA], [HB|TB]) ->
  maybe
    ok ?= unify(Env, HA, HB),
    unify(Env, TA, TB)
  end;
unify_dereferenced(_, [], []) ->
  ok;
unify_dereferenced(Env, A, B) when is_map(A) andalso is_map(B) andalso map_size(A) == map_size(B) ->
  unify_map(Env, A, B);
unify_dereferenced(Env, A, B) when is_tuple(A) andalso is_tuple(B) andalso tuple_size(A) == tuple_size(B) ->
  unify_tuple(Env, tuple_size(A), 1, A, B);
unify_dereferenced(_, _, _) ->
  {error, unification_failed}.
