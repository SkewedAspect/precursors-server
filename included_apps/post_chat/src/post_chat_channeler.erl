-module(post_chat_channeler).

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/pre_client.hrl").
% api
-export([start_link/0,client_login_hook/2]).
% channel stuf
-export([client_request/4,client_response/4,client_event/3]).
% supervisor stuff
-export([init/1]).

%% public api
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [supervisor]).

client_login_hook(undefined, Client) ->
	?info("Client logged in, registering chat channel"),
	supervisor:start_child(?MODULE, [Client#client_info.channel_manager]),
	Join = post_chatroom:join(<<"global">>, Client),
	?info("Join: ~p", [Join]),
	{ok, undefined}.

%% supervisor
init([supervisor]) ->
	process_flag(trap_exit, true),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]),
	Kid = {id, {pre_client_channels, set_sup_channel, [<<"chat">>, ?MODULE, undefined]}, transient, brutal_kill, worker, [?MODULE]},
	{ok, {{simple_one_for_one, 3, 5}, [Kid]}}.

%% client channels
client_event(Client, {struct, Event}, undefined) ->
	RoomId = proplists:get_value(<<"room">>, Event, <<>>),
	Command = proplists:get_value(<<"action">>, Event),
	?info("Chat Message: ~p", [Event]),
	case Command of
		<<"leave">> ->
			room_call(leave, RoomId, Client, []);
		<<"join">> ->
			Pass = proplists:get_value(<<"password">>, Event, <<>>),
			room_call(join, RoomId, Client, [Pass]);
		<<"message">> ->
			Message = proplists:get_value(<<"message">>, Event),
			room_call(message, RoomId, Client, Message);
		<<"create_room">> ->
			Password = proplists:get_value(<<"password">>, Event, <<>>),
			Mode = case proplists:get_value(<<"mode">>, Event) of
				<<"plugin">> -> plugin;
				_ -> player
			end,
			room_call(create_room, RoomId, Client, [Mode, Password]);
			Targeted when
					Targeted =:= <<"kick">>;
					Targeted =:= <<"mute">>;
					Targeted =:= <<"unmute">>;
					Targeted =:= <<"promote">>;
					Targeted =:= <<"demote">> ->
			Target = proplists:get_value(<<"name">>, Event),
			Verb = case Targeted of
				<<"kick">> -> kick;
				<<"mute">> -> mute;
				<<"unmute">> -> mute;
				<<"promote">> -> promote;
				<<"demote">> -> demote
			end,
			room_call(Verb, RoomId, Client, [Target]);
%		leave(Room, Client) ->
%			gen_server:call(Room, {leave, Client}, ?timeout).
%		join(Room, Client) ->
%			join(Room, Client, "").
%		join(Room, Client, Password) ->
%			gen_server:call(Room, {join, Client, Password}, ?timeout).
%		kick(Room, Client, Target) ->
%			gen_server:call(Room, {kick, Client, Target}, ?timeout).
%		mute(Room, Client, Target) ->
%			gen_server:call(Room, {mute, Client, Target}, ?timeout).
%		unmute(Room, Client, Target) ->
%			gen_server:call(Room, {unmute, Client, Target}, ?timeout).
%		message(Room, Client, Message) ->
%			gen_server:call(Room, {message, Client, Message}, ?timeout).
		_ ->
			{ok, undefined}
	end.


client_request(Client, _Id, {struct, Request}, undefined) ->
	RoomId = proplists:get_value(<<"room">>, Request, <<>>),
	Command = proplists:get_value(<<"action">>, Request),
	?info("Chat Message: ~p", [Request]),
	case Command of
		<<"leave">> ->
			room_call(leave, RoomId, Client, []);
		<<"join">> ->
			Pass = proplists:get_value(<<"password">>, Request, <<>>),
			room_call(join,RoomId,Client,[Pass]);
		<<"message">> ->
			Message = proplists:get_value(<<"message">>, Request),
			room_call(message,RoomId,Client,[Message]);
		<<"create_room">> ->
			Password = proplists:get_value(<<"password">>, Request, <<>>),
			Mode = case proplists:get_value(<<"mode">>, Request) of
				<<"plugin">> -> plugin;
				_ -> player
			end,
			room_call(create_room,RoomId,Client,[Mode,Password]);
		Targeted when
					Targeted =:= <<"kick">>;
					Targeted =:= <<"mute">>;
					Targeted =:= <<"unmute">>;
					Targeted =:= <<"promote">>;
					Targeted =:= <<"demote">> ->
			Target = proplists:get_value(<<"name">>, Request),
			Verb = case Targeted of
				<<"kick">> -> kick;
				<<"mute">> -> mute;
				<<"unmute">> -> mute;
				<<"promote">> -> promote;
				<<"demote">> -> demote
			end,
			room_call(Verb,RoomId,Client,[Target]);
%		leave(Room, Client) ->
%			gen_server:call(Room, {leave, Client}, ?timeout).
%		join(Room, Client) ->
%			join(Room, Client, "").
%		join(Room, Client, Password) ->
%			gen_server:call(Room, {join, Client, Password}, ?timeout).
%		kick(Room, Client, Target) ->
%			gen_server:call(Room, {kick, Client, Target}, ?timeout).
%		mute(Room, Client, Target) ->
%			gen_server:call(Room, {mute, Client, Target}, ?timeout).
%		unmute(Room, Client, Target) ->
%			gen_server:call(Room, {unmute, Client, Target}, ?timeout).
%		message(Room, Client, Message) ->
%			gen_server:call(Room, {message, Client, Message}, ?timeout).
		_ ->
			{reply, {struct, [{<<"success">>, false}, {<<"message">>, <<"unknown command">>}]}}
	end.
	
room_call(message, RoomID, Client, Message) ->
	?info("Msg: ~p, ~p, ~p", [RoomID, Client, Message]),
	post_chatroom:message(RoomID, Client, Message),
	{ok, undefined};
%	{reply, {struct, [{<<"success">>, true}, {<<"message">>, Message}]}};

room_call(create_room,RoomName,Client,[Mode,Password]) ->
%	Qh = qlc:q([X || #room_cache{name = N} = X <- ets:table(?ets),
%		N =:= RoomName]),
%	case qlc:e(Qh) of
%		[] ->
%			case post_chatroom_sup
	{reply, {struct, [{<<"success">>, false},{<<"message">>,<<"nyi">>}]}};
room_call(_Func,_RoomId,_Client,_Args) ->
	{reply, {struct, [{<<"success">>, false},{<<"message">>,<<"nyi">>}]}}.

client_response(_Client, _Id, _Response, undefined) ->
	{ok, undefined}.

