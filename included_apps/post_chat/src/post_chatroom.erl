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

-define(timeout, 1000).
-define(ets, post_chatrooms).

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/pre_client.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
	start_link/3,
	start_link/4,
	send_message/3,
	leave/2,
	join/2,
	join/3,
	kick/3,
	mute/3,
	unmute/3,
	message/3
]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-record(state, {
	name :: string(),
	chatters = [] :: [{pid(), string()}],
	controllers :: [pid()],
	squelched = [] :: [pid()],
	mode = user :: 'user' | 'system' | 'plugin',
	password,
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
	?info("Chatroom start_link called: ~p ~p ~p", [Name, Mode, ModeMeta]),
	start_link(Name, Mode, ModeMeta, undefined).

start_link(Name, Mode, ModeMeta, Password) ->
	?info("Chatroom starting: ~p ~p ~p ~p", [Name, Mode, ModeMeta, Password]),
	Res = qlc:e(qlc:q([
		E || {_ListPid, _Pid, RoomName, RoomMode} = E <- ets:table(post_chatrooms),
		RoomName =:= Name, RoomMode =:= Mode
	])),
    ?info("Matching: ~p ~p", [Res, Mode]),
	case {Res, Mode} of
		{[], player} ->
  		PlayPID = gen_server:start_link({global, Name}, ?MODULE, [{Name, Mode, ModeMeta, Password}], []),
		?info("Player Room started: ~p", [PlayPID]);
		{[{_Lpid, RPid, _} | _], player} ->
		?info("Other Room started: ~p", [RPid]),
			{ok, RPid};
		{_, system} ->
  		NewPID = gen_server:start_link({global, Name}, ?MODULE, {Name, Mode, ModeMeta, Password}, []),
		?info("System Room started: ~p", [NewPID]),
        NewPID
	end.

send_message(Pid, Command, Payload) ->
	gen_server:cast(Pid, {message, Command, Payload}).

% and so we can issue messages more easily.  The general idiom is:
% function(RoomPid, IssuingClientInfo, Command[, Arg1..., ArgN]) ->
% 	das res

leave(Room, Client) ->
	gen_server:call({global, Room}, {leave, Client}, ?timeout).

join(Room, Client) ->
	join(Room, Client, undefined).

join(Room, Client, Password) ->
	gen_server:call({global, Room}, {join, Client, Password}, ?timeout).

kick(Room, Client, Target) ->
	gen_server:call({global, Room}, {kick, Client, Target}, ?timeout).

mute(Room, Client, Target) ->
	gen_server:call({global, Room}, {mute, Client, Target}, ?timeout).

unmute(Room, Client, Target) ->
	gen_server:call({global, Room}, {unmute, Client, Target}, ?timeout).

message(Room, Client, Message) ->
	?info("Message to send: ~p ~p ~p", [Room, Client, Message]),
	gen_server:call({global, Room}, {message, Client, Message}, ?timeout).



%% ==================================================================
%% gen_server
%% ==================================================================

init({Name, Mode, ModeMeta, Password}) ->
	State = #state{name = Name, mode = Mode, password = Password},
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

pid_to_bin(P) ->
	list_to_binary(pid_to_list(P)).

broadcast(_Message, []) ->
	ok;
broadcast(Message, [{C,_} | Tail]) ->
	pre_client_connection:send(C, tcp, event, <<"chat">>, jsx:to_json(Message)),
	broadcast(Message, Tail).

remove_chatter(Conn, Name, Chatters, Controllers, RoomPid) ->
	Controllers0 = lists:delete(Conn, Controllers),
	Chatters0 = lists:filter(fun({P,_}) -> P =/= Conn end, Chatters),
	MidOut = case {Controllers0, Chatters0} of
		{[], [{New, NewName} | _]} ->
			Message = {struct, [
				{<<"message">>, <<"new_controller">>},
				{<<"room">>, pid_to_bin(RoomPid)},
				{<<"controller">>, NewName}
			]},
			broadcast(Message, Chatters0),
			{[New], Chatters0};
		Out -> Out
	end,
	LeaveMsg = {struct, [
		{<<"message">>, <<"chatter_left">>},
		{<<"room">>, pid_to_bin(RoomPid)},
		{<<"leaver">>, Name}
	]},
	broadcast(LeaveMsg, Chatters0),
	MidOut.

handle_call({leave, _Client}, _From, #state{mode = system, mode_meta =
	#system_opts{leavable = no_leavers}} = State) ->
	{reply, {error, no_leavers}, State};

handle_call({leave, Client}, _From, State) ->
	#state{chatters = Chatters, controllers = Controllers} = State,
	#client_info{connection = Conn} = Client,
	case [N || {P,N} <- Chatters, P =:= Conn] of
		[] ->
			{reply, {error, not_member}, State};
		[Name] ->
			{Chatters0, Controllers0} = remove_chatter(Conn, Name, Chatters, Controllers, self()),
			case {Chatters0, State#state.mode} of
				{_, system} ->
					{reply, ok, State#state{chatters = Chatters0, controllers = Controllers0}};
				{[], _PluginOrUser} ->
					{stop, normal, ok, State#state{chatters = Chatters0, controllers = Controllers0}};
				{_, plugin} ->
					{reply, ok, State#state{chatters = Chatters0, controllers = []}};
				{_, user} ->
					{reply, ok, State#state{chatters = Chatters0, controllers = Controllers0}}
			end
	end;

handle_call({join, Client, Password}, _From, #state{password = Stateword} = State)
	when Stateword =/= undefined orelse Password =:= Stateword ->
	#state{chatters = Chatters} = State,
	#client_info{connection = Conn, username = Name} = Client,
	Chatters0 = [{Conn, Name} | Chatters],
	Msg = {struct, [
		{<<"message">>, <<"chatter_joined">>},
		{<<"room">>, pid_to_bin(self())},
		{<<"joiner">>, Name}
	]},
	broadcast(Msg, Chatters0),
	{reply, ok, State#state{chatters = Chatters0}};

handle_call({join, _Client, _Password}, _From, State) ->
	{reply, {error, bad_password}, State};

handle_call({kick, _Client, _Target}, _From, #state{mode = system, mode_meta = #system_opts{kicking = no_kicking}} = State) ->
	{reply, {error, no_kicking}, State};

handle_call({kick, Client, Target}, From, State) ->
	#client_info{connection = Conn} = Client,
	case lists:member(Conn, State#state.controllers) of
		false ->
			{reply, {error, not_controller}, State};
		true ->
			% code reuse is fun!
			case lists:keyfind(Target, 2, State#state.chatters) of
				false ->
					{reply, {error, no_target}, State};
				TargetPid ->
					FakeClient = Client#client_info{connection = TargetPid},
					handle_call({leave, FakeClient}, From, State)
			end
	end;

handle_call({mute, _Client, _Target}, _From, #state{mode = system, mode_meta = #system_opts{muting = no_mute}} = State) ->
	{reply, {error, no_muting}, State};

handle_call({mute, Client, Target}, _From, State) ->
	#client_info{connection = Conn} = Client,
	TargetPid = lists:keyfind(Target, 2, State#state.chatters),
	IsChatter = lists:member(TargetPid, State#state.chatters),
	IsController = lists:member(Conn, State#state.controllers),
	case {IsChatter, IsController} of
		{false, _} ->
			{reply, {error, no_target}, State};
		{_, false} ->
			{reply, {error, not_controller}, State};
		{_,_} ->
			Squelched = [TargetPid | State#state.squelched],
			Msg = {struct, [
				{<<"message">>, <<"muted">>},
				{<<"room">>, pid_to_bin(self())}
			]},
			broadcast(Msg, [{TargetPid, "name"}]),
			{reply,ok,State#state{squelched = Squelched}}
	end;

handle_call({unmute, Client, Target}, _From, State) ->
	TargetPid = lists:keyfind(Target, 2, State#state.chatters),
	#state{squelched = Squelched} = State,
	#client_info{connection = Conn} = Client,
	IsController = lists:member(Conn, State#state.controllers),
	case {IsController, lists:delete(TargetPid, Squelched)} of
		{false, _} ->
			{reply, {error, not_controller}, State};
		{true, Squelched} ->
			{reply, {error, not_muted}, State};
		{true, NewSquelched} ->
			State0 = State#state{squelched = NewSquelched},
			{reply, ok, State0}
	end;

handle_call({message, Client, Message}, _From, State) ->
	#state{chatters = Chatters, squelched = Squelched} = State,
	#client_info{connection = ConnPid, username = Username} = Client,
	case lists:member(ConnPid, Squelched) of
		true ->
			{reply, {error, muted}, State};
		false ->
			Json = {struct, [
				{<<"action">>, <<"message">>},
				{<<"room">>, pid_to_bin(self())},
				{<<"payload">>, Message},
				{<<"user">>, Username}
			]},
			Pid = proc_lib:spawn(fun() ->
						?info("Entered spawn with ~p", [Chatters]),
						[begin ?info("Sending: ~p tp ~p", [Json, Pid]), pre_client_connection:send(Pid, tcp, event, <<"chat">>, Json) end ||
					{Pid, _} <- Chatters]
			end),
			?info("Handled message ~p ~p ~p", [Client, Message, Pid]),
			{reply, ok, State}
	end;

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

