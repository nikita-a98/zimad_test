%%%-------------------------------------------------------------------
%%% @author Nikita Lysiuk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2019 1:42 AM
%%%-------------------------------------------------------------------
-module(zimad).
-author("nikita").

%% API
-export([
  register/1,
  authorize/1,
  get_profile/1,
  del_profile/1,
  buy_stars/2,
  win_level/1
]).

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
-spec register(Nickname) -> {ok, Uid} | {error, Reason} when
  Nickname  :: binary(),
  Uid       :: binary(),
  Reason    :: term().

register(Nickname) ->
  F =
    fun() ->
      case mnesia:read({?TAB_ZIMAD_USER_NICKNAMES, Nickname}) of
        [_] ->
          {error, already_exists};
        _ ->
          Uid = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
          RecNickname = #zimad_user_nickname{
            user_id = Uid,
            nickname = Nickname
          },
          mnesia:write(RecNickname),
          RecProfile = #zimad_user{
            user_id = Uid,
            coins = 100 %% Hardcoded value
          },
          mnesia:write(RecProfile),
          {ok, #{<<"uid">> => Uid}}
      end
    end,
  case mnesia:transaction(F) of
    {'atomic', Res} -> Res;
    {'aborted', Reason} -> {error, Reason}
  end.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec authorize(Uid) -> {ok, Token} | {error, Reason} when
  Uid       :: binary(),
  Token     :: binary(),
  Reason    :: term().

authorize(Uid) ->
  case mnesia:dirty_read({?TAB_ZIMAD_USERS, Uid}) of
    [_] ->
      case mnesia:dirty_index_read(?TAB_ZIMAD_JWT, Uid, #zimad_jwt.user_id) of
        [#zimad_jwt{token = Token}] ->
          ok = mnesia:dirty_delete({?TAB_ZIMAD_JWT, Token});
        [] ->
          ok
      end,
      zimad_auth_token:create_jwt(Uid);
    _ ->
      {error, not_found}
  end.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec get_profile(Uid) -> {ok, Profile} | {error, Reason} when
  Uid       :: binary(),
  Profile   :: maps:map(),
  Reason    :: term().

get_profile(Uid) ->
  case mnesia:dirty_read({?TAB_ZIMAD_USERS, Uid}) of
    [Rec] ->
      #zimad_user{
        user_id = Uid,
        coins = Coins,
        stars = Stars,
        level = Level
      } = Rec,
      case mnesia:dirty_index_read(?TAB_ZIMAD_USER_NICKNAMES, Uid, #zimad_user_nickname.user_id) of
        [#zimad_user_nickname{nickname = Nickname}] ->
          {ok, #{
            <<"user_id">> => Uid,
            <<"nickname">> => Nickname,
            <<"coins">> => Coins,
            <<"stars">> => Stars,
            <<"level">> => Level
          }};
        _ ->
          {error, not_found}
      end;
    _ ->
      {error, not_found}
  end.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec del_profile(Uid) -> ok when
  Uid       :: binary().

del_profile(Uid) ->
  ok = mnesia:dirty_delete({?TAB_ZIMAD_USERS, Uid}),
  case mnesia:dirty_index_read(?TAB_ZIMAD_USER_NICKNAMES, Uid, #zimad_user_nickname.user_id) of
    [#zimad_user_nickname{nickname = Nickname}] ->
      ok = mnesia:dirty_delete({?TAB_ZIMAD_USER_NICKNAMES, Nickname});
    [] ->
      ok
  end,
  case mnesia:dirty_index_read(?TAB_ZIMAD_JWT, Uid, #zimad_jwt.user_id) of
    [#zimad_jwt{token = Token}] ->
      ok = mnesia:dirty_delete({?TAB_ZIMAD_JWT, Token});
    [] ->
      ok
  end.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec win_level(Uid) -> ok | {error, Reason} when
  Uid       :: binary(),
  Reason    :: term().

win_level(Uid) ->
  F =
    fun() ->
      case mnesia:read({?TAB_ZIMAD_USERS, Uid}) of
        [#zimad_user{level = Level}=Rec] ->
          NewLevel = Level+1,
          mnesia:write(Rec#zimad_user{level = NewLevel}),
          ok;
        _ ->
          {error, not_found}
      end
    end,
  case mnesia:transaction(F) of
    {'atomic', Res} -> Res;
    {'aborted', Reason} -> {error, Reason}
  end.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec buy_stars(Uid, CountStars) -> ok | {error, Reason} when
  Uid        :: binary(),
  CountStars :: pos_integer(),
  Reason     :: term().

buy_stars(Uid, CountStars) ->
  F =
    fun() ->
      case mnesia:read({?TAB_ZIMAD_USERS, Uid}) of
        [#zimad_user{coins = Coins, stars = Stars}=Rec] ->
          case CountStars * 10 of
            CoinsNeed when Coins >= CoinsNeed ->
              ResidualCoin = Coins - CoinsNeed,
              NewStarsCount = Stars + CountStars,
              mnesia:write(Rec#zimad_user{coins = ResidualCoin, stars = NewStarsCount}),
              ok;
            _ ->
              {error, not_enough_coin}
          end;
        _ ->
          {error, not_found}
      end
    end,
  case mnesia:transaction(F) of
    {'atomic', Res} -> Res;
    {'aborted', Reason} -> {error, Reason}
  end.