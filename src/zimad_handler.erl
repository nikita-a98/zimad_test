%%%-------------------------------------------------------------------
%%% @author Nikita Lysiuk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2019 3:31 AM
%%%-------------------------------------------------------------------
-module(zimad_handler).
-author("nikita").

%% Cowboy callbacks
-export([
  init/2,
  terminate/3,
  allowed_methods/2,
  is_authorized/2,
  content_types_provided/2,
  content_types_accepted/2
]).

-export([
  to_json/2
]).

%%% ==================================================================
%%% Cowboy callbacks
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec init(Req, any()) -> {ok | module(), Req, any()} | {module(), Req, any(), any()} when
  Req :: cowboy_req:req().

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec terminate(Reason, Req, State) -> ok when
  Reason :: normal | {crash, error | exit | throw, any()},
  Req :: cowboy_req:req(),
  State :: term().

terminate(_Reason, _Req, _State) ->
  ok.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec allowed_methods(Req, State) -> {Result, Req, State} when
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: [Method],
  Method :: binary(),
  Req :: cowboy_req:req(),
  State :: term().

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec is_authorized(Req, State) -> {Result, Req, State} when
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: true | {false, AuthHeader :: iodata()},
  Req :: cowboy_req:req(),
  State :: term().

is_authorized(Req, State) ->
  case lists:filter(fun(E) -> E /= <<>> end, binary:split(cowboy_req:path(Req), <<"/">>, [global])) of
    [<<"user">>, <<"get_profile">> | _] ->
      is_auth(Req, State);
    [<<"user">>, <<"del_profile">> | _] ->
      is_auth(Req, State);
    [<<"level">>, <<"win">> | _] ->
      is_auth(Req, State);
    [<<"stars">>, <<"buy">> | _] ->
      is_auth(Req, State);
    _ ->
      {true, Req, State}
  end.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec content_types_provided(Req, State) -> {Result, Req, State} when
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: [{binary() | ParsedMime, ProvideCallback :: atom()}],
  ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
  Params :: [{Key :: binary(), Value :: binary()}],
  Req :: cowboy_req:req(),
  State :: term().

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, '*'}, to_json}
  ], Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec content_types_accepted(Req, State) -> {Result, Req, State} when
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: [{binary() | ParsedMime, AcceptCallback :: atom()}],
  ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
  Params :: [{Key :: binary(), Value :: binary()}],
  Req :: cowboy_req:req(),
  State :: term().

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, '*'}, to_json}
  ], Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec to_json(Req, State) -> {Result, Req, State} when
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: cowboy_req:resp_body().

to_json(Req, State) ->
  Method = cowboy_req:method(Req),
  to_json(Method, Req, State).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec to_json(Method, Req, State) -> {Result, Req, State} when
  Method :: binary(),
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: cowboy_req:resp_body().

to_json(<<"POST">>, Req, State) ->
  case cowboy_req:has_body(Req) of
    true ->
      {ok, ReqBodyRaw, _} = cowboy_req:read_body(Req),
      case catch jiffy:decode(ReqBodyRaw, [return_maps]) of
        ReqBody when is_map(ReqBody); is_list(ReqBody) ->
          {ApiCall, Ctx} = init_call_ctx(Req, State),
          ApiCall2 = maps:put(<<"data">>, ReqBody, ApiCall),
          {RespBody, State2} = zimad_api:call(ApiCall2, Ctx),
          Json = jiffy:encode(RespBody),
          Req2 = cowboy_req:reply(200, #{}, Json, Req),
          {stop, Req2, State2};
        _ ->
          RespBody = zimad_api:to_response(invalid_json),
          Json = jiffy:encode(RespBody),
          Req2 = cowboy_req:reply(200, #{}, Json, Req),
          {stop, Req2, State}
      end;
    false ->
      {ApiCall, Ctx} = init_call_ctx(Req, State),
      {RespBody, State2} = zimad_api:call(ApiCall, Ctx),
      Json = jiffy:encode(RespBody),
      Req2 = cowboy_req:reply(200, #{}, Json, Req),
      {stop, Req2, State2}
  end;

to_json(<<"GET">>, Req, State) ->
  {ApiCall, Ctx} = init_call_ctx(Req, State),
  {RespBody, State2} = zimad_api:call(ApiCall, Ctx),
  Json = jiffy:encode(RespBody),
  Req2 = cowboy_req:reply(200, #{}, Json, Req),
  {stop, Req2, State2}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec init_call_ctx(Req, State) -> {ApiCall, Ctx} when
  Req :: cowboy_req:req(),
  State :: maps:map(),
  ApiCall :: maps:map(),
  Ctx :: maps:map().

init_call_ctx(Req, State) ->
  {init_call(Req), init_ctx(State)}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec init_call(Req) -> ApiCall when
  Req :: cowboy_req:req(),
  ApiCall :: maps:map().

init_call(Req) ->
  [Obj, Op | _] = lists:filter(fun(E) -> E /= <<>> end, binary:split(cowboy_req:path(Req), <<"/">>, [global])),
  List = list_key_vals_to_bin(cowboy_req:parse_qs(Req)),
  Map = maps:from_list(List),
  Data = maps:without([<<"key">>], Map),
  case maps:size(Data) of
    0 ->
      #{
        <<"obj">> => Obj,
        <<"op">> => Op
      };
    _ ->
      #{
        <<"obj">> => Obj,
        <<"op">> => Op,
        <<"data">> => Data
      }
  end.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec init_ctx(State) -> Ctx when
  State :: maps:map(),
  Ctx :: maps:map().

init_ctx(State) ->
  maps:put(<<"req_time">>, erlang:system_time(seconds), State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec is_auth(Req, State) -> true | {false, Reason} when
  Req :: cowboy_req:req(),
  State :: maps:map(),
  Reason :: binary().

is_auth(Req, State) ->
  List = list_key_vals_to_bin(cowboy_req:parse_qs(Req)),
  Res =
    case List of
      [{<<"key">>, Token}|_] ->
        case zimad_auth_token:check_jwt(Token) of
          {ok, #{claims := #{<<"uid">> := Uid}}} ->
            State1 = maps:put(<<"uid">>, Uid, #{}),
            {true, Req, State1};
          {error, Reason1} ->
            {error, Reason1}
        end;
      _ ->
        {error, invalid_jwt}
    end,
  case Res of
    {true, Req, State2} ->
      {true, Req, State2};
    {error, Reason} ->
      Resp = zimad_api:to_response(Reason),
      Error = maps:get(<<"error_id">>, Resp),
      Description = maps:get(<<"message">>, Resp),
      WwwAuthenticate = <<"Bearer realm=\"Zimad\", error=\"", Error/binary, " \", error_desription=\"", Description/binary, "\"">>,
      {{false, WwwAuthenticate}, Req, State}
  end.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec list_key_vals_to_bin(L1) -> L2 when
  L1 :: proplists:proplist(),
  L2 :: proplists:proplist().

list_key_vals_to_bin(L) ->
  F =
    fun({K, V}, AccIn) ->
      case catch to_bin(K) of
        K2 when is_binary(K2) ->
          case catch to_bin(V) of
            V2 when is_binary(V2) ->
              [{K2, V2} | AccIn];
            _ ->
              AccIn
          end;
        _ ->
          AccIn
      end
    end,
  lists:foldl(F, [], L).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec to_bin(X) -> B when
  X :: binary() | atom() | integer() | list(),
  B :: binary().

to_bin(X) when is_binary(X) ->
  X;

to_bin(X) when is_list(X) ->
  erlang:list_to_binary(X);

to_bin(X) when is_atom(X) ->
  erlang:atom_to_binary(X, utf8);

to_bin(X) when is_integer(X) ->
  erlang:integer_to_binary(X).