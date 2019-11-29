%%%-------------------------------------------------------------------
%%% @author Nikita Lysiuk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2019 12:15 AM
%%%-------------------------------------------------------------------
-module(zimad_api).
-author("nikita").

%% API
-export([
  call/2,
  to_response/1
]).

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec call(Req, Ctx1) -> {Resp, Ctx2} when
  Req :: maps:map(),
  Ctx1 :: maps:map(),
  Ctx2 :: maps:map(),
  Resp :: {maps:map(), maps:map()}.

call(#{<<"obj">> := <<"user">>, <<"op">> := <<"register">>, <<"data">> := Data}, Ctx) ->
  case Data of
    #{<<"nickname">> := Nickname} ->
      Res = zimad:register(Nickname),
      {to_response(Res), Ctx};
    _ ->
      {to_response(bad_request), Ctx}
  end;

call(#{<<"obj">> := <<"user">>, <<"op">> := <<"authorize">>, <<"data">> := Data}, Ctx) ->
  case Data of
    #{<<"uid">> := Uid} ->
      Res = zimad:authorize(Uid),
      {to_response(Res), Ctx};
    _ ->
      {to_response(bad_request), Ctx}
  end;

call(#{<<"obj">> := <<"user">>, <<"op">> := <<"get_profile">>}, Ctx) ->
  Uid = maps:get(<<"uid">>, Ctx),
  Res = zimad:get_profile(Uid),
  {to_response(Res), Ctx};

call(#{<<"obj">> := <<"user">>, <<"op">> := <<"del_profile">>}, Ctx) ->
  Uid = maps:get(<<"uid">>, Ctx),
  Res = zimad:del_profile(Uid),
  {to_response(Res), Ctx};

call(#{<<"obj">> := <<"level">>, <<"op">> := <<"win">>}, Ctx) ->
  Uid = maps:get(<<"uid">>, Ctx),
  Res = zimad:win_level(Uid),
  {to_response(Res), Ctx};

call(#{<<"obj">> := <<"stars">>, <<"op">> := <<"buy">>, <<"data">> := Data}, Ctx) ->
  case Data of
    #{<<"stars_count">> := Count} ->
      Uid = maps:get(<<"uid">>, Ctx),
      CountInt = binary_to_integer(Count),
      Res = zimad:buy_stars(Uid, CountInt),
      {to_response(Res), Ctx};
    _ ->
      {to_response(bad_request), Ctx}
  end;

call(_, Ctx) ->
  {to_response(forbidden), Ctx}.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec to_response(Val) -> Response when
  Val :: ok | {ok, Data} | {error, Reason} | Reason,
  Data :: term(),
  Reason :: atom(),
  Response :: #{status => term(), code => pos_integer(), data := maps:map() | binary()}.

to_response(ok) ->
  #{<<"status">> => <<"ok">>};

to_response({ok, Data}) ->
  #{<<"status">> => <<"ok">>, <<"data">> => Data};

to_response({error, Reason}) ->
  to_response(Reason);

to_response(bad_request) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"bad_request">>,
    <<"message">> => <<"Bad Request">>
  };

to_response(unauthorized) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"unauthorized">>,
    <<"message">> => <<"Unauthorized">>
  };

to_response(forbidden) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"forbidden">>,
    <<"message">> => <<"Forbidden">>
  };

to_response(not_found) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"not_found">>,
    <<"message">> => <<"Not Found">>
  };

to_response(already_exists) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"already_exists">>,
    <<"message">> => <<"Conflict">>
  };

to_response(jwt_expired) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"jwt_expired">>,
    <<"message">> => <<"JWT Expired">>
  };

to_response(jwt_inactive_yet) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"jwt_inactive_yet">>,
    <<"message">> => <<"JWT Inactive Yet">>
  };

to_response(jwt_issued_in_future) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"jwt_issued_in_future">>,
    <<"message">> => <<"JWT Issued In Future">>
  };

to_response(invalid_sign) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"invalid_sign">>,
    <<"message">> => <<"Invalid Sign">>
  };

to_response(invalid_jwt) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"invalid_jwt">>,
    <<"message">> => <<"JWT Not Found">>
  };

to_response(not_enough_coin) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"not_enough_coin">>,
    <<"message">> => <<"Not Enough Coin">>
  };

to_response(_) ->
  #{
    <<"status">> => <<"error">>,
    <<"error_id">> => <<"internal_server_error">>,
    <<"message">> => <<"Internal Server Error">>
  }.
