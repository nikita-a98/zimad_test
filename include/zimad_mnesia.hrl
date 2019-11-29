%%%-------------------------------------------------------------------
%%% @author Nikita Lysiuk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2019 11:28 PM
%%%-------------------------------------------------------------------
-author("nikita").

%%% ==================================================================
%%% Macro
%%% ==================================================================

-define(MNESIA_NODES, nodes()).

%%% ==================================================================
%%% Records
%%% ==================================================================

-record(zimad_jwt, {
  token   :: binary(),
  user_id :: binary(),
  exp     :: pos_integer()
}).

-define(TAB_ZIMAD_JWT, zimad_jwt).
-define(TAB_ZIMAD_JWT_DEF,
  [
    {access_mode, read_write},
    {disc_copies, [node()]},
    {type, set},
    {attributes, record_info(fields, ?TAB_ZIMAD_JWT)},
    {majority, true},
    {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
    {index, [user_id, exp]}
  ]).

%% -------------------------------------------------------------------

-record(zimad_user, {
  user_id      :: binary(),
  coins = 100  :: pos_integer(),
  stars = 0    :: pos_integer(),
  level = 0    :: pos_integer()
}).

-define(TAB_ZIMAD_USERS, zimad_user).
-define(TAB_ZIMAD_USERS_DEF,
  [
    {access_mode, read_write},
    {disc_copies, [node()]},
    {type, ordered_set},
    {attributes, record_info(fields, ?TAB_ZIMAD_USERS)},
    {majority, true},
    {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
    {index, []}
  ]).

%% -------------------------------------------------------------------

-record(zimad_user_nickname, {
  nickname :: binary(),
  user_id  :: binary()
}).

-define(TAB_ZIMAD_USER_NICKNAMES, zimad_user_nickname).
-define(TAB_ZIMAD_USER_NICKNAMES_DEF,
  [
    {access_mode, read_write},
    {disc_copies, [node()]},
    {type, ordered_set},
    {attributes, record_info(fields, ?TAB_ZIMAD_USER_NICKNAMES)},
    {majority, true},
    {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
    {index, [user_id]}
  ]).

%% -------------------------------------------------------------------

-define(MNESIA_TABS_DEFS,
  [
    {?TAB_ZIMAD_JWT, ?TAB_ZIMAD_JWT_DEF},
    {?TAB_ZIMAD_USERS, ?TAB_ZIMAD_USERS_DEF},
    {?TAB_ZIMAD_USER_NICKNAMES, ?TAB_ZIMAD_USER_NICKNAMES_DEF}
  ]).

-define(MNESIA_TABS_LIST, [TabName || {TabName, _} <- ?MNESIA_TABS_DEFS]).