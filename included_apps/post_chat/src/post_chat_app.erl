-module(post_chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	GlobalChat = {<<"global">>, system, [shown, no_leavers, no_kick, no_mute], undefined},
	Rooms = case application:get_env(post_chat, rooms) of
		undefined -> [];
		{ok, R} -> R
	end,
	post_chat_sup:start_link([GlobalChat | Rooms]).

stop(_State) ->
	ok.
