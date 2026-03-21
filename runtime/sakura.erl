-module(sakura).

-export([ask/2]).

-include("sakura.hrl").

serialize(Query) -> Query. % TODO
deserialize(Response) -> {ok, Response}. % TODO
create_session(_Socket, _Query) -> {ok, dummy}. % TODO

get_response(Socket, ReceiveTimeout, Session) ->
  maybe
    ok ?= gen_tcp:send(Socket, Session),
    {ok, RawData} ?= gen_tcp:recv(Socket, 0, ReceiveTimeout),
    {ok, Response} ?= deserialize(RawData),
    case Response of
      'end' -> [];
      {more, Value} -> [Value | fun () -> get_response(Socket, ReceiveTimeout, Session) end]
    end
  else
    {error, Error} -> error(Error)
  end.

ask(Pattern, RawQuery) ->
  fun(#{db_config := #{socket := Socket, timeouts := TimeoutMap}} = State) ->
    ReceiveTimeout = maps:get('receive', TimeoutMap, 5 * 1000),
    maybe
      Query = serialize(RawQuery),
      {ok, Session} ?= create_session(Socket, Query),
      Results = karuta:bind_results(Pattern, fun () -> get_response(Socket, ReceiveTimeout, Session) end),
      Results(State)
    else
      {error, Error} -> error(Error)
    end
  end.
