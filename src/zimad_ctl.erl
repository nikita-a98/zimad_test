%%%-------------------------------------------------------------------
%%% @author Nikita Lysiuk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2019 3:14 AM
%%%-------------------------------------------------------------------
-module(zimad_ctl).
-author("nikita").

-behaviour(gen_server).

%% API
-export([
  start_http/0,
  stop_http/0
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

-define(GEN_SERVER_OPTS, []).

-define(HTTP_REST_API_REF, http).
-define(EV_HTTP_SERVICE, init_http_service).

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec start_http() -> ok | {error, Reason} when
  Reason :: term().

start_http() ->
  Ip = {127,0,0,1}, %% Hardcoded value
  Host = '_', %% Hardcoded value
  Port = 8080, %% Hardcoded value
  Ref = ?HTTP_REST_API_REF, %% Hardcoded value
  Urls = [{"/:obj/:op", zimad_handler, #{}}], %% Hardcoded value
  Dispatch = cowboy_router:compile([{Host, Urls}]),
  TransOpts = [{ip, Ip}, {port, Port}],
  ProtoOpts = #{env => #{dispatch => Dispatch}},
  case cowboy:start_clear(Ref, TransOpts, ProtoOpts) of
    {ok, Pid} when is_pid(Pid) ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%% -------------------------------------------------------------------
%% @doc
%% Stops HTTP API.
%% @end
%% -------------------------------------------------------------------
-spec stop_http() -> ok | {error, Reason} when
  Reason :: term().

stop_http() ->
  cowboy:stop_listener(?HTTP_REST_API_REF).

%% -------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%% -------------------------------------------------------------------
-spec start_link() ->
  {ok, Pid :: pid()}
  | ignore
  | {error, Reason :: term()}.

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

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, ?GEN_SERVER_OPTS).

%%% ==================================================================
%%% gen_server callbacks
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% @end
%% -------------------------------------------------------------------
-spec init(Args :: term()) ->
  {ok, State :: list()}
  | {ok, State :: list(), timeout() | hibernate}
  | {stop, Reason :: term()}
  | ignore.

init([]) ->
  self() ! ?EV_HTTP_SERVICE,
  {ok, []}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%% @end
%% -------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: list()) ->
  {reply, Reply :: term(), NewState :: list()}
  | {reply, Reply :: term(), list(), timeout() | hibernate}
  | {noreply, list()}
  | {noreply, list(), timeout() | hibernate}
  | {stop, Reason :: term(), Reply :: term(), list()}
  | {stop, Reason :: term(), list()}.

handle_call(_, _, State) ->
  {reply, ok, State}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%% @end
%% -------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: list()) ->
  {noreply, NewState :: list()}
  | {noreply, NewState :: list(), timeout() | hibernate}
  | {stop, Reason :: term(), NewState :: list()}.

handle_cast(_, State) ->
  {noreply, State}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% @end
%% -------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: list()) ->
  {noreply, NewState :: list()}
  | {noreply, NewState :: list(), timeout() | hibernate}
  | {stop, Reason :: term(), NewState :: list()}.

handle_info(?EV_HTTP_SERVICE, State) ->
  case start_http() of
    ok ->
      {noreply, State};
    {error, {already_started, _Pid}} ->
      {noreply, State};
    {error, Reason} ->
      {stop, Reason, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%% -------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: list()) -> term().

terminate(_Reason, _State) ->
  ok.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%% -------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: list(), Extra :: term()) ->
  {ok, NewState :: list()}
  | {error, Reason :: term()}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
