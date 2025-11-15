-module(karuta).

-export([make_environment/0, environment_behavior/2, test/0]).

test_pop() ->
  receive V ->
    io:format("Received: ~p\n", [V]),
    V
  end.

test() ->
  Env = make_environment(),
  Env ! {fresh, self()},
  {fresh_variable, A} = test_pop(),
  Env ! {new_choice, self()},
  test_pop(),
  Env ! {discard, self()},
  {discard, Wildcard} = test_pop(),
  Env ! {unify, [A|Wildcard], [1, 2, 3], self()},
  test_pop(),
  Env ! {deref, A, self()},
  test_pop(),
  Env ! {deref, Wildcard, self()},
  test_pop(),
  Env ! {backtrack, self()},
  test_pop(),
  Env ! {deref, A, self()},
  test_pop().

make_environment() -> spawn(?MODULE, environment_behavior, [#{}, []]).

environment_behavior(Bindings, ChoicePoints) ->
  receive
    {unify, LHS, RHS, Pid} ->
      maybe
        {ok, NewBindings} ?= unify(Bindings, LHS, RHS),
        Pid ! {ok, variable_bound},
        environment_behavior(NewBindings, ChoicePoints)
      else
        Error ->
          Pid ! {error, Error},
           environment_behavior(Bindings, ChoicePoints)
      end;
    {fresh, Pid} ->
      Var = make_ref(),
      Pid ! {fresh_variable, Var},
      environment_behavior(Bindings#{Var => unbound}, ChoicePoints);
    {discard, Pid} ->
      case Bindings of
       #{discard := Discard} ->
          Pid ! {discard, Discard},
          environment_behavior(Bindings, ChoicePoints);
       _ ->
          Discard = make_ref(),
          Pid ! {discard, Discard},
          environment_behavior(
            Bindings#{discard => Discard, Discard => discard},
            ChoicePoints
          )
      end;
    {deref, Var, Pid} ->
      Pid ! {dereferenced, deref(Bindings, Var)},
      environment_behavior(Bindings, ChoicePoints);
    {new_choice, Pid} ->
      Pid ! {ok, choice_point_created},
      environment_behavior(Bindings, [Bindings | ChoicePoints]);
    {backtrack, Pid} ->
      maybe
        [PreviousBindings|PreviousChoicePoints] ?= ChoicePoints,
        Pid ! {ok, backtracked},
        environment_behavior(PreviousBindings, PreviousChoicePoints)
      else
        _ ->
          Pid ! {error, no_choice_points},
          environment_behavior(Bindings, ChoicePoints)
      end;
    M ->
      io:format("Unknown message: ~p\n", [M]),
      environment_behavior(Bindings, ChoicePoints)
  end.

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
