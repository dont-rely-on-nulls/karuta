-module(sakura).

-export([ask/2]).

-include("sakura.hrl").

serialize_primitive(Primitive) when is_binary(Primitive)-> ["\"", Primitive, "\""];
serialize_primitive(Primitive) when is_integer(Primitive)-> integer_to_list(Primitive);
serialize_primitive(Primitive) when is_float(Primitive)-> float_to_list(Primitive);
serialize_primitive(Primitive) when is_boolean(Primitive)-> atom_to_list(Primitive).

serialize_tag_value(TagValue) ->
    case TagValue of
        int -> "Int";
        float -> "Float";
        str -> "Str";
        bool -> "Bool"
    end.

serialize_value({Tag, Primitive}) ->
    ["(", serialize_tag_value(Tag), " ", serialize_primitive(Primitive), ")"].

serialize_symbol(Key) when is_binary(Key) -> serialize_primitive(Key);
serialize_symbol(Key) when is_atom(Key) -> atom_to_list(Key).

serialize_map(Map, SerializeValue) ->
  [maps:fold(
    fun (Key, Value, Acc) -> 
            SerializedKey = serialize_symbol(Key),
            SerializedValue = SerializeValue(Value),
            [Acc, "(", SerializedKey, " ", SerializedValue,")"]
    end, "(", Map), ")"].

serialize_list_of_symbols(ListOfSymbols) -> lists:map(fun serialize_symbol/1, ListOfSymbols).

serialize_drl_body(Query) -> 
    case Query of
        {base, RelationName}     -> ["(Base ", serialize_symbol(RelationName), ")"];
        {const, Const}           -> ["(Const ", serialize_map(Const, fun serialize_value/1), " )"]; 
        {select, Query1, Query2} -> ["(Select ", serialize_drl_body(Query1), " ", serialize_drl_body(Query2), ")"];
        {join, ListOfSymbols, Query1, Query2} -> 
            ["(Join ", serialize_list_of_symbols(ListOfSymbols), " ", 
             serialize_drl_body(Query1), " ", serialize_drl_body(Query2), ")"];
        {cartesian, Query1, Query2} -> ["(Cartesian ", serialize_drl_body(Query1), " ", serialize_drl_body(Query2), ")"];
        {project, ListOfSymbols, Query1} -> ["(Project ", serialize_list_of_symbols(ListOfSymbols), " ", serialize_drl_body(Query1), ")"];
        {rename, RenameMap, Query1} -> ["(Rename ", serialize_map(RenameMap, fun serialize_symbol/1), " ", serialize_drl_body(Query1), " )"]; 
        {union, Query1, Query2} -> ["(Union ", serialize_drl_body(Query1), " ", serialize_drl_body(Query2), ")"];
        {diff, Query1, Query2} -> ["(Diff ", serialize_drl_body(Query1), " ", serialize_drl_body(Query2), ")"];
        {take, HowMany, Query1} -> ["(Take ", serialize_primitive(HowMany), " ", serialize_drl_body(Query1), ")"]
    end.

serialize_scl_body({'begin', Query}) ->
    DrlSerializedBody = serialize_drl_body(Query),
    ["(Begin (query ", DrlSerializedBody, ")", " (limit (0)))"];

serialize_scl_body({fetch, Cursor}) -> ["(Fetch (cursor ", Cursor, ")", " (limit (1)))"].
   
serialize_sublanguage(Tag) ->
    case Tag of 
        drl -> {"drl", fun serialize_drl_body/1};
        ddl -> {"ddl", fun serialize_drl_body/1};
        dml -> {"dml", fun serialize_drl_body/1};
        scl -> {"scl", fun serialize_scl_body/1};
        Unsupported -> error(["We do not support the following sublanguage yet:", atom_to_list(Unsupported)])
    end.
             
serialize({Tag, Query}) ->
    {SerializedTag, SerializeBodyFun} = serialize_sublanguage(Tag),
    {ok, ["(", SerializedTag, " ", SerializeBodyFun(Query), ")"]}.

deserialize({drl, _Response}) -> 
    error("TODO: Wait for the LFE migration");
deserialize({scl, session, _Response}) -> 
    error("TODO: Wait for the LFE migration");

deserialize({scl, data, Response}) -> deserialize({drl, Response});

deserialize(Response) -> {ok, Response}. % TODO

create_session(Socket, ReceiveTimeout, Query) -> 
    maybe        
        {ok, SessionCreationPayload} ?= serialize({scl, Query}),
        ok ?= gen_tcp:send(Socket, SessionCreationPayload),
        {ok, RawData} ?= gen_tcp:recv(Socket, 0, ReceiveTimeout),
        % TODO: How do we identify that this receive is about the above send? We need to have some nonce about it.
        DeserializedSession ?= deserialize({scl, session, RawData}),
        {ok, maps:get(DeserializedSession, cursor)}
    else
        {error, Error} -> error(Error)
    end.

get_response(Socket, ReceiveTimeout, SessionID) ->
  maybe
      {ok, SerializedFetch} ?= serialize({scl, {fetch, SessionID}}),
      ok ?= gen_tcp:send(Socket, SerializedFetch),
      {ok, RawData} ?= gen_tcp:recv(Socket, 0, ReceiveTimeout),
      {ok, #{has_more := More, row := Rows}} ?= deserialize(RawData),
      case Rows of
          [] -> [];
          [Row] -> 
              case More of
                  true -> [Row | fun () -> get_response(Socket, ReceiveTimeout, SessionID) end];
                  false -> [Row]
              end;
          _ -> error("Unreachable: ask primitive should only fetch one piece of data at a time")
      end
  else
    {error, Error} -> error(Error)
  end.

ask(Pattern, RawQuery) ->
  fun(#{db_config := #{socket := Socket, timeouts := TimeoutMap}} = State) ->
    ReceiveTimeout = maps:get('receive', TimeoutMap, 5 * 1000),
    maybe
      {ok, Query} ?= serialize({drl, RawQuery}),
      {ok, SessionID} ?= create_session(Socket, ReceiveTimeout, Query),
      Results = karuta:bind_results(Pattern, fun () -> get_response(Socket, ReceiveTimeout, SessionID) end),
      Results(State)
    else
      {error, Error} -> error(Error)
    end
  end.
