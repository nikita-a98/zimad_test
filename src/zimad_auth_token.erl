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

-define(KEY, <<"-----BEGIN RSA PRIVATE KEY-----\nMIICWgIBAAKBgEMkaqhx3QjGA2+ofqaKtOl+8kRsPUQpqKYMVRf3cF4Dlak9pa1a\n3AeJFzaWN1iFszhLICNSjGqlE5zeyAvCElG4yQ6DfCDH8fgYxgkx1GHSe8Pnd6NB\npVMk+qm/eoCY1WfKaihrxM0SOnsHDtybkPJ1SYVzt9KaRIWe5+mCEznbAgMBAAEC\ngYABjiBiUnDaOSvvNCnq1Z+nIOefqhopMAXWAscdzP9vTgnzZ5gk0cvy6Lv+6a/S\nfxZNoX/XEZxrA/PWWFqt2wrtj5UgK1vHfGBt4bRfofJqJ4RJxlF63PG6G+bljaPK\nr4QMuWhuQ7oTo9vpJX3d9VlJQ6PA/A6pjEd6C0KgIYNmsQJBAIVMxADwoMcathM2\nNyAnR9De254yCuw25rMxUSJjqUkP1biJN2bPTO9TygfNvBzZXT1oxobqUmiLP03F\n3bvK+B0CQQCA8guxb7M+lBgs+zB0fjOmzvH3GYx1CWXC5x6CjdkdZGRKWRIL/AQG\npRae9P45XRY+HVobvroMcxCQbgm6OAhXAkAbN2pJmtfo7y4/y/EDcqG6JtaGfUc3\npZoC+k3LjLeywDXt4K5cInVd5Ci64SnIZiUgdbUbNNqwl8XMFltIjY+NAkBYx86b\nPaA5TvgSSGTFYHspt8TLcHDPyEejWUQdeZLqdMvEkO0nCT6wYIxhp6c+UcRVwhnf\njoNEvRjDiK/z4k6DAkB37MF42ZKcuzkYZz3loYAa6E7SGRApGXWugX3tN7OXoGpY\n4uO9N1a/oIOgYX+mmBm9wISNW+pGtWj8RIrYC93E\n-----END RSA PRIVATE KEY-----\n">>).

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
  Alg = <<"RS256">>, %% Hardcoded value
  Ttl = 900, %% Hardcoded value
  Jti = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
  AccessJwtClaims = #{
    uid => UserId,
    jti => Jti
  },
  AccessJwtOpts = [
    {ttl, Ttl},
    {key, ?KEY},
    {alg, Alg}
  ],
  #{jwt := AccessJwt, exp := AccessJwtExp} = create(AccessJwtClaims, AccessJwtOpts),
  Rec = #zimad_jwt{
    token = AccessJwt,
    user_id = UserId,
    exp = AccessJwtExp
  },
  ok = mnesia:dirty_write(Rec),
  {ok, #{<<"token">> => AccessJwt}}.

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

sign(<<"RS256">>, Key, JwtPart) ->
  [Entry] = public_key:pem_decode(Key),
  PeKey = public_key:pem_entry_decode(Entry),
  public_key:sign(JwtPart, sha256, PeKey);

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

verify_sign(<<"RS256">>, Key, Sign, JwtPart) ->
  [Entry] = public_key:pem_decode(Key),
  PeKey = public_key:pem_entry_decode(Entry),
  case public_key:verify(JwtPart, sha256, Sign, PeKey) of
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