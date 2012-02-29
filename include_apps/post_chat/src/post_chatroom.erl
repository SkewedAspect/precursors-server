%% @doc A chatroom useful for humans, the server, or client plugin
%% communication.  When a chatroom is started, it is given a mode and
%% meta data for the mode.  The three modes are `user', `system', and
%% `plugin'.
%%
%% == `user' ==
%%
%% A user created chatroom.  The metadata is the `#client_info{}' of the
%% user who created the room, as well as an optional password.  They are
%% automatically a controller.
%%
%% There must always be at least one controller.  If the last controller
%% leaves the room, a new controller is chosen from the chatters still
%% in the room.  If there are no chatters, the room's process exits
%% gracefully.
%%
%% == `system' ==
%%
%% A room created by the system, such as a guild chat room.  Users cannot
%% create or remove system chat rooms.  They may be created or removed
%% from the system dynamically.
%%
%% The metadata for a system room is a list of options:
%% <ul>
%% <li>hidden:  The channel is not advertised for joining.</li>
%% <li>no_leavers:  A user cannot leave the channel.</li>
%% <li>no_kick:  A user cannot kick other users.</li>
%% <li>no_mute:  A user cannot mute other users.</li>
%% </ul>
%%
%% == `plugin' ==
%%
%% A plugin room is so a client plugin can communicate with itself when
%% running on other clients.  The meta data is the #client_info{} of the
%% user which is running the plugin.  They are the first chatter.  When
%% there are no more chatters, the chatroom is destroyed.

-module(post_chatroom).
-behaviour(gen_server).

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/pre_client.hrl").
%% API
-export([start_link/3]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-record(state, {
	name :: string(),
	chatters :: [pid()],
	controllers :: [pid()],
	mode = user :: 'user' | 'system' | 'plugin',
	mode_meta
}).

-record(system_opts, {
	display = shown :: 'shown' | 'hidden',
	leavable = leavers :: 'leavers' | 'no_leavers',
	kicking = kick :: 'kick' | 'no_kick',
	muting = mute :: 'mute' | 'no_mute'
}).

%% ==================================================================
%% API 
%% ==================================================================

%% @doc Creates a new chat room.  Name is the display name of the channel
%% given to users.  Mode is the type of chat room.  ModeMeta depends on
%% the mode.
start_link(Name, Mode, ModeMeta) ->
  gen_server:start_link(?MODULE, [{Name, Mode, ModeMeta}], []).

send_command(Pid, Command, Payload) ->
	gen_server:cast(Pid, {command, Command, Payload}).

%% ==================================================================
%% gen_server
%% ==================================================================

init({Name, Mode, ModeMeta}) ->
	State = #state{name = Name, mode = Mode},
	case Mode of
		user when is_record(ModeMeta, client_info) ->
			#client_info{connection = Conn} = ModeMeta,
			Chatters = [ModeMeta],
			Controller = [Conn],
			State0 = State#state{chatters = Chatters, controllers = Controller},
			{ok, State0};
		plugin when is_record(ModeMeta, client_info) ->
			#client_info{connection = Conn} = ModeMeta,
			Chatters = [Conn],
			State0 = State#state{chatters = Chatters},
			{ok, State0};
		system when is_list(ModeMeta) ->
			SystemOpts = build_system_opts(ModeMeta),
			State0 = State#state{mode_meta = SystemOpts},
			{ok, State0}
	end.

build_system_opts(Opts) ->
	build_system_opts(Opts, #system_opts{}).

build_system_opts([], Rec) ->
	Rec;

build_system_opts([hidden | Tail], Rec) ->
	Rec0 = Rec#system_opts{display = hidden},
	build_system_opts(Tail, Rec0);

build_system_opts([shown | Tail], Rec) ->
	Rec0 = Rec#system_opts{display = shown},
	build_system_opts(Tail, Rec0);

build_system_opts([leavers | Tail], Rec) ->
	Rec0 = Rec#system_opts{leavable = leavers},
	build_system_opts(Tail, Rec0);

build_system_opts([no_leavers | Tail], Rec) ->
	Rec0 = Rec#system_opts{leavable = no_leavers},
	build_system_opts(Tail, Rec0);

build_system_opts([kick | Tail], Rec) ->
	Rec0 = Rec#system_opts{kicking = kick},
	build_system_opts(Tail, Rec0);

build_system_opts([no_kick | Tail], Rec) ->
	Rec0 = Rec#system_opts{kicking = no_kick},
	build_system_opts(Tail, Rec0);

build_system_opts([mute | Tail], Rec) ->
	Rec0 = Rec#system_opts{muting = mute},
	build_system_opts(Tail, Rec0);

build_system_opts([no_mute | Tail], Rec) ->
	Rec0 = Rec#system_opts{muting = no_mute},
	build_system_opts(Tail, Rec0).

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

%% ------------------------------------------------------------------
%% handle_cast
%% ------------------------------------------------------------------

handle_cast(_Msg, State) ->
  {noreply, State}.

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

handle_info(_Info, State) ->
  {noreply, State}.

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

%% ------------------------------------------------------------------
%% code_change
%% ------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ==================================================================
%% Internal Function Definitions
%% ==================================================================

