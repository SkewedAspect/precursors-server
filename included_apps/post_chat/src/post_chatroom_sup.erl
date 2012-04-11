-module(post_chatroom_sup).

-behaviour(supervisor).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/1,get_room/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(ets, post_chatrooms).

-record(room_cache, {
	id :: string(), % pid_to_list(room_chache.pid)
	pid :: pid(),
	name :: string(),
	unique :: 'unique' | string()
}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Rooms) ->
	{ok, Pid} = Out = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	[begin
		Args = tuple_to_list(Room),
		supervisor:start_child(Pid, Args)
	end || Room <- Rooms],
	Out.

-spec get_room(Key :: 'id' | 'name', Id :: string()) -> 'undefined' | pid().
get_room(Key, Id) ->
	QH = case Key of
		id ->
			qlc:q([P || #room_cache{pid = P, id = I} <- ets:table(?ets),
				I =:= Id]);
		name ->
			qlc:q([P || #room_cache{pid = P, name = N} <- ets:table(?ets),
				N =:= Id])
	end,
	case qlc:e(QH) of
		[] -> undefined;
		[X] -> X
	end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Child = {id, {post_chatroom, start_link, []}, permanent, brutal_kill,worker,[post_chatroom]},
	{ok, { simple_one_for_one, 5, 10}, [Child]}.
