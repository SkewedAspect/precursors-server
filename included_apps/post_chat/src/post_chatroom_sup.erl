-module(post_chatroom_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Child = {id, {post_chatroom, start_link, []}, permanent, brutal_kill,worker,[post_chatroom]},
	{ok, { simple_one_for_one, 5, 10}, [Child]}.
