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

-behaviour(gen_server).

%% API
-export([
  register/1,
  authorize/1,
  get_profile/1,
  gdpr_erase_profile/1,
  buy_stars/2,
  win_level/1
]).

%% API
-export([
  start_link/0,
  start_link/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("zimad_mnesia.hrl").

%%% ==================================================================
%%% Macro
%%% ==================================================================

-define(SERVER, ?MODULE).

%%% ==================================================================
%%% Records
%%% ==================================================================

-record(state, {}).

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% User registration
%% @end
%% -------------------------------------------------------------------
-spec register(Nickname) -> Uid when
  Nickname  :: binary(),
  Uid       :: binary().

register(Nickname) ->
  gen_server:call(?SERVER, {register, Nickname}).

%% -------------------------------------------------------------------
%% @doc
%% User authorization
%% @end
%% -------------------------------------------------------------------
-spec authorize(Uid) -> Token when
  Uid       :: binary(),
  Token     :: binary().

authorize(Uid) ->
  gen_server:call(?SERVER, {authorize, Uid}).

%% -------------------------------------------------------------------
%% @doc
%% Get user profile
%% @end
%% -------------------------------------------------------------------
-spec get_profile(Token) -> Json when
  Token     :: binary(),
  Json      :: binary().

get_profile(Token) ->
  gen_server:call(?SERVER, {get_profile, Token}).

%% -------------------------------------------------------------------
%% @doc
%% Delete user profile
%% @end
%% -------------------------------------------------------------------
-spec gdpr_erase_profile(Token) -> Json when
  Token     :: binary(),
  Json      :: binary().

gdpr_erase_profile(Token) ->
  gen_server:call(?SERVER, {gdpr_erase_profile, Token}).

%% -------------------------------------------------------------------
%% @doc
%% Game level victory
%% @end
%% -------------------------------------------------------------------
-spec win_level(Token) -> ok when
  Token     :: binary().

win_level(Token) ->
  gen_server:call(?SERVER, {win_level, Token}).

%% -------------------------------------------------------------------
%% @doc
%% Buy stars
%% @end
%% -------------------------------------------------------------------
-spec buy_stars(Token, CountStars) -> Json when
  Token      :: binary(),
  CountStars :: pos_integer(),
  Json       :: binary().

buy_stars(Token, CountStars) ->
  gen_server:call(?SERVER, {buy_stars, Token, CountStars}).



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
  start_link([]).

%% -------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%% -------------------------------------------------------------------
-spec start_link(Args :: term()) ->
  {ok, Pid :: pid()}
  | ignore
  | {error, Reason :: term()}.

start_link([]) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({register, Nickname}, _From, State) ->
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
          {ok, Uid}
      end
    end,
  Res =
    case mnesia:transaction(F) of
      {'atomic', Result} ->
        Result;
      {'aborted', TReason} ->
        {error, TReason}
    end,
  Resp =
    case Res of
      {ok, Uid} ->
        Uid;
      {error, Reason} ->
        #{<<"error">> => atom_to_binary(Reason, utf8)}
    end,
  {reply, jiffy:encode(Resp), State};

handle_call({authorize, Uid}, _From, State) ->
  Res =
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
    end,
  Resp =
    case Res of
      {ok, AuthToken} ->
        AuthToken;
      {error, Reason} ->
        #{<<"error">> => atom_to_binary(Reason, utf8)}
    end,
  {reply, jiffy:encode(Resp), State};

handle_call({get_profile, Token}, _From, State) ->
  Res =
    case zimad_auth_token:check_jwt(Token) of
      {ok, #{claims := #{<<"uid">> := Uid}}} ->
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
              _ -> {error, not_found}
            end;
          _ -> {error, not_found}
        end;
      Error -> Error
    end,
  Resp =
    case Res of
      {ok, Profile} ->
        Profile;
      {error, Reason} ->
        #{<<"error">> => atom_to_binary(Reason, utf8)}
    end,
  {reply, jiffy:encode(Resp), State};

handle_call({gdpr_erase_profile, Token}, _From, State) ->
  Res =
    case zimad_auth_token:check_jwt(Token) of
      {ok, #{claims := #{<<"uid">> := Uid}}} ->
        case mnesia:dirty_index_read(?TAB_ZIMAD_JWT, Uid, #zimad_jwt.user_id) of
          [#zimad_jwt{token = Token}] ->
            ok = mnesia:dirty_delete({?TAB_ZIMAD_JWT, Token});
          [] ->
            ok
        end,
        ok = mnesia:dirty_delete({?TAB_ZIMAD_USERS, Uid}),
        case mnesia:dirty_index_read(?TAB_ZIMAD_USER_NICKNAMES, Uid, #zimad_user_nickname.user_id) of
          [#zimad_user_nickname{nickname = Nickname}] ->
            ok = mnesia:dirty_delete({?TAB_ZIMAD_USER_NICKNAMES, Nickname});
          [] ->
            already_deleted
        end;
      Error -> Error
    end,
  Resp =
    case Res of
      {error, Reason} ->
        #{<<"error">> => atom_to_binary(Reason, utf8)};
      Status ->
        #{<<"status">> => atom_to_binary(Status, utf8)}
    end,
  {reply, jiffy:encode(Resp), State};

handle_call({win_level, Token}, _From, State) ->
  Res =
    case zimad_auth_token:check_jwt(Token) of
      {ok, #{claims := #{<<"uid">> := Uid}}} ->
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
          {'atomic', Result} -> Result;
          {'aborted', Reason} -> {error, Reason}
        end;
      Error -> Error
    end,
  Resp =
    case Res of
      ok ->
        #{<<"status">> => <<"ok">>};
      {error, Reason1} ->
        #{<<"error">> => atom_to_binary(Reason1, utf8)}
    end,
  {reply, jiffy:encode(Resp), State};

handle_call({buy_stars, Token, CountStars}, _From, State) ->
  Res =
    case zimad_auth_token:check_jwt(Token) of
      {ok, #{claims := #{<<"uid">> := Uid}}} ->
        F =
          fun() ->
            case mnesia:read({?TAB_ZIMAD_USERS, Uid}) of
              [#zimad_user{coins = Coins, stars = Stars}=Rec] ->
                case CountStars * 10 of
                  CoinsNeed when Coins >= CoinsNeed ->
                    ResidualCoin = Coins - CoinsNeed,
                    NewStarsCount = Stars + CountStars,
                    mnesia:write(Rec#zimad_user{coins = ResidualCoin, stars = NewStarsCount}),
                    {ok, NewStarsCount};
                  _ ->
                    {error, not_enough_coin}
                end;
              _ ->
                {error, not_found}
            end
          end,
        case mnesia:transaction(F) of
          {'atomic', Result} -> Result;
          {'aborted', Reason} -> {error, Reason}
        end;
      Error -> Error
    end,
  Resp =
    case Res of
      {ok, Stars} ->
        #{<<"status">> => <<"ok">>, <<"stars">> => Stars};
      {error, Reason1} ->
        #{<<"error">> => atom_to_binary(Reason1, utf8)}
    end,
  {reply, jiffy:encode(Resp), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info(_Info, State) ->
  {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
