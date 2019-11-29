%%%-------------------------------------------------------------------
%%% @author Nikita Lysiuk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2019 12:00 AM
%%%-------------------------------------------------------------------
-module(zimad_app).
-author("nikita").

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("zimad_mnesia.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason} when
  StartType   :: normal | {takeover, Node} | {failover, Node},
  Node        :: node(),
  StartArgs   :: term(),
  Pid         :: pid(),
  State       :: term(),
  Reason      :: term().

start(_StartType, _StartArgs) ->
  mnesia:stop(),
  mnesia:create_schema([node()]),
  mnesia:start(),
  F =
    fun({Tab, TabDef}, AccIn) ->
      case mnesia:create_table(Tab, TabDef) of
        {atomic, ok} ->
          AccIn;
        {aborted, Reason} ->
          [Reason | AccIn]
      end
    end,
  lists:foldl(F, [], ?MNESIA_TABS_DEFS),
  zimad_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> Resp when
  State :: term(),
  Resp :: term().

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
