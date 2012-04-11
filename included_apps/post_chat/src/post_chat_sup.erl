-module(post_chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Rooms) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Rooms).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Rooms) ->
	ets:new(post_chatrooms, [named_table, public, {keypos, 2}]),

	Chatrooms = ?CHILD(post_chatrooms_sup, supervisor, Rooms),

	ChannelMgr = ?CHILD(post_chat_channeler, supervisor, Rooms),

	Children = [Chatrooms, ChannelMgr],

	{ok, {one_for_one, 5, 10}, Children}.
