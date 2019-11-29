%%%-------------------------------------------------------------------
%%% @author Nikita Lysiuk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2019 11:53 PM
%%%-------------------------------------------------------------------
-module(zimad_sup).
-author("nikita").

-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

%%%===================================================================
%%% Macro
%%%===================================================================

-define(SERVER, ?MODULE).

-define(WORKER(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, brutal_kill, worker, [Mod]}).
-define(WORKER(Mod), ?WORKER(Mod, [])).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> {ok, {SupFlags, Childs}} | ignore | {error, Reason} when
  Args            :: term(),
  SupFlags        :: {RestartStrategy, MaxR, MaxT},
  RestartStrategy :: supervisor:strategy(),
  MaxR            :: non_neg_integer(),
  MaxT            :: non_neg_integer(),
  Childs          :: list(supervisor:child_spec()),
  Reason          :: term().

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 1,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Childs = [
    ?WORKER(zimad_ctl, [])
  ],

  {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
