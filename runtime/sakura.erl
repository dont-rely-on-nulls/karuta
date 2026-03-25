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
        {select, Query1, Query2} -> ["(Select ", serialize(Query1), " ", serialize(Query2), ")"];
        {join, ListOfSymbols, Query1, Query2} -> 
            ["(Join ", serialize_list_of_symbols(ListOfSymbols), " ", 
             serialize(Query1), " ", serialize(Query2), ")"];
        {cartesian, Query1, Query2} -> ["(Cartesian ", serialize(Query1), " ", serialize(Query2), ")"];
        {project, ListOfSymbols, Query1} -> ["(Project ", serialize_list_of_symbols(ListOfSymbols), " ", serialize(Query1), ")"];
        {rename, RenameMap, Query1} -> ["(Rename ", serialize_map(RenameMap, fun serialize_symbol/1), " ", serialize(Query1), " )"]; 
        {union, Query1, Query2} -> ["(Union ", serialize(Query1), " ", serialize(Query2), ")"];
        {diff, Query1, Query2} -> ["(Diff ", serialize(Query1), " ", serialize(Query2), ")"];
        {take, HowMany, Query1} -> ["(Take ", serialize_primitive(HowMany), " ", serialize(Query1), ")"]
    end.

serialize_scl_body({'begin', Query}) ->
    DrlSerializedBody = serialize_drl_body(Query),
    ["(Begin (query ", DrlSerializedBody, ")", " (limit (0)))"].
    
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

deserialize(Response) -> {ok, Response}. % TODO

create_session(Socket, ReceiveTimeout, Query) -> 
    maybe        
        {ok, SessionCreationPayload} ?= serialize({scl, Query}),
        ok ?= gen_tcp:send(Socket, SessionCreationPayload),
        {ok, RawData} ?= gen_tcp:recv(Socket, 0, ReceiveTimeout),
        % TODO: How do we identify that this receive is about the above send? We need to have some nonce about it.
        deserialize({scl, RawData})
    else
        {error, Error} -> error(Error)
    end.

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
      {ok, Query} ?= serialize({drl, RawQuery}),
      {ok, Session} ?= create_session(Socket, ReceiveTimeout, Query),
      Results = karuta:bind_results(Pattern, fun () -> get_response(Socket, ReceiveTimeout, Session) end),
      Results(State)
    else
      {error, Error} -> error(Error)
    end
  end.
