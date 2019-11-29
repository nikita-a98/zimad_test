%%%-------------------------------------------------------------------
%%% @author Nikita Lysiuk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2019 2:09 AM
%%%-------------------------------------------------------------------
-module(zimad_auth_token).
-author("nikita").

%% API
-export([
  create_jwt/1,
  check_jwt/1
]).

%%% ==================================================================
%%% Macros
%%% ==================================================================

-define(TTL, 900).
-define(ALG, <<"HS256">>).
-define(KEY, <<"OC5XeKtn0-pS-kU8KxF41h7WzMMd-IY12Xj6xboX7rk">>).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("zimad_mnesia.hrl").

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec create_jwt(UserId) -> {ok, Token} when
  UserId :: binary(),
  Token  :: binary().

create_jwt(UserId) ->
  Jti = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
  AccessJwtClaims = #{
    uid => UserId,
    jti => Jti
  },
  AccessJwtOpts = [
    {ttl, ?TTL},
    {key, ?KEY},
    {alg, ?ALG}
  ],
  #{jwt := AccessJwt, exp := AccessJwtExp} = create(AccessJwtClaims, AccessJwtOpts),
  Rec = #zimad_jwt{
    token = AccessJwt,
    user_id = UserId,
    exp = AccessJwtExp
  },
  ok = mnesia:dirty_write(Rec),
  {ok, AccessJwt}.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec check_jwt(Token) -> ok | {error, Reason} when
  Token  :: binary(),
  Reason :: term().

check_jwt(Token) ->
  case parse(Token, [{key, ?KEY}]) of
    {ok, Vals} ->
      case mnesia:dirty_read({?TAB_ZIMAD_JWT, Token}) of
        [_] ->
          {ok, Vals};
        _ ->
          {error, invalid_jwt}
      end;
    {error, jwt_expired} ->
      ok = mnesia:dirty_delete({?TAB_ZIMAD_JWT, Token}),
      {error, jwt_expired};
    {error, Reason} ->
      {error, Reason}
  end.


%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec create(JwtClaims, Opts) -> JwtInfo | {error, Reason} when
  JwtClaims :: maps:map(),
  JwtInfo :: #{jwt := binary(), exp := pos_integer(), iat := pos_integer()},
  Opts :: [Opt],
  Opt :: {alg, binary()}
  | {ttl, pos_integer()}
  | {nbf, pos_integer()}
  | {key, binary()},
  Reason :: invalid_alg | term().

create(JwtClaims, Opts) ->
  Alg = proplists:get_value(alg, Opts),
  Now = erlang:system_time(seconds) - 1,
  JwtHeader = #{alg => Alg, typ => <<"JWT">>},
  Key = proplists:get_value(key, Opts),
  Ttl = proplists:get_value(ttl, Opts),
  Exp = Now + Ttl,
  H = encode(jiffy:encode(JwtHeader)),
  JwtClaims2 = #{
    iat => Now,
    exp => Exp
  },
  JwtClaims3 = maps:merge(JwtClaims, JwtClaims2),
  C = encode(jiffy:encode(JwtClaims3)),
  JwtPart = <<H/binary, ".", C/binary>>,
  S = encode(sign(Alg, Key, JwtPart)),
  #{
    jwt => <<JwtPart/binary, ".", S/binary>>,
    exp => Exp,
    iat => Now
  }.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec encode(JwtPart1) -> JwtPart2 when
  JwtPart1 :: binary(),
  JwtPart2 :: binary().

encode(JwtPart) when is_binary(JwtPart) ->
  JwtPart2 = base64:encode(JwtPart),
  JwtPart3 = binary:replace(JwtPart2, <<"=">>, <<>>, [global]),
  << << (base64urlencode(X)) >> || <<X>> <= JwtPart3 >>.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec sign(Alg, Key, JwtPart) -> Sign when
  Alg :: binary(),
  Key :: binary(),
  JwtPart :: binary(),
  Sign :: binary().

sign(<<"HS256">>, Key, JwtPart) ->
  crypto:hmac(sha256, Key, JwtPart);

sign(_Alg, _Key, _JwtPart) ->
  {error, badarg}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec parse(Jwt, Opts) -> {ok, JwtData} | {error, Reason} when
  Jwt :: binary(),
  Opts :: [Opt],
  Opt :: {key, binary()},
  JwtData :: maps:map(),
  Reason :: invalid_jwt | invalid_sign | no_key | badarg.

parse(Jwt, Opts) when is_binary(Jwt), is_list(Opts) ->
  case binary:split(Jwt, <<".">>, [global]) of
    [H, P, S] ->
      H2 = << << (base64urldecode(X)) >> || <<X>> <= H >>,
      P2 = << << (base64urldecode(X)) >> || <<X>> <= P >>,
      H3 = base64:decode(normalize_base64(H2)),
      P3 = base64:decode(normalize_base64(P2)),
      H4 = jiffy:decode(H3, [return_maps]),
      P4 = jiffy:decode(P3, [return_maps]),

      Alg = maps:get(<<"alg">>, H4, undefined),
      Data = <<H2/binary, ".", P2/binary>>,
      Sign = base64:decode(normalize_base64(<< << (base64urldecode(X)) >> || <<X>> <= S >>)),
      Key = proplists:get_value(key, Opts),

      SignStatus =
        case Key of
          undefined -> {error, no_key};
          Key ->
            case verify_sign(Alg, Key, Sign, Data) of
              ok -> ok;
              Any -> Any
            end
        end,
      ClaimsStatus = check_claims(P4),

      case {SignStatus, ClaimsStatus} of
        {ok, ok} ->
          Val = #{
            header => H4,
            claims => P4,
            sign => Sign
          },
          {ok, Val};
        {{error, Reason}, _} ->
          {error, Reason};
        {_, {error, Reason}} ->
          {error, Reason}
      end;
    _ ->
      {error, invalid_jwt}
  end;

parse(_, _) ->
  {error, badarg}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec verify_sign(JwtHeader, Key, Sign, JwtPart) -> ok | {error, Reason} when
  JwtHeader :: maps:map(),
  Key :: binary(),
  Sign :: binary(),
  JwtPart :: binary(),
  Reason :: badarg | invalid_sign.

verify_sign(<<"HS256">>, Key, Sign, JwtPart) ->
  case (Sign == crypto:hmac(sha256, Key, JwtPart)) of
    true ->
      ok;
    false ->
      {error, invalid_sign}
  end;

verify_sign(_Alg, _Key, _Sign, _JwtPart) ->
  {error, badarg}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec check_claims(JwtClaims) -> ok | {error, Reasons} when
  JwtClaims :: maps:map(),
  Reasons :: [Reason],
  Reason :: jwt_expired | jwt_inactive_yet | jwt_issued_in_future.

check_claims(JwtClaims) when map_size(JwtClaims) /= 0 ->
  Now = erlang:system_time(seconds),
  F =
    fun
      (<<"exp">>, V, AccIn) ->
        case (is_integer(V) andalso V < Now) of
          true ->
            [jwt_expired | AccIn];
          false ->
            AccIn
        end;
      (<<"nbf">>, V, AccIn) ->
        case (is_integer(V) andalso V < Now) of
          true ->
            [jwt_inactive_yet | AccIn];
          false ->
            AccIn
        end;
      (<<"iat">>, V, AccIn) ->
        case (is_integer(V) andalso V > 1 andalso V =< Now) of
          true ->
            AccIn;
          false ->
            [jwt_issued_in_future | AccIn]
        end;
      (_, _, AccIn) ->
        AccIn
    end,
  case maps:fold(F, [], JwtClaims) of
    [] ->
      ok;
    [Error|_] ->
      {error, Error}
  end.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec normalize_base64(B1) -> B2 when
  B1 :: binary(),
  B2 :: binary().

normalize_base64(B) ->
  case byte_size(B) rem 4 of
    2 ->
      <<B/binary, "==">>;
    3 ->
      <<B/binary, "=">>;
    _ ->
      B
  end.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec base64urlencode(C1) -> C2 when
  C1 :: char(),
  C2 :: char().

base64urlencode($/) -> $_;
base64urlencode($+) -> $-;
base64urlencode(C) -> C.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec base64urldecode(C1) -> C2 when
  C1 :: char(),
  C2 :: char().

base64urldecode($_) -> $/;
base64urldecode($-) -> $+;
base64urldecode(C) -> C.