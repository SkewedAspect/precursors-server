%% @doc Holds the channels for an agent.  Backed by gen_event.  This is
%% intended to replace pre_gen_client.

-module(pre_client_channels).
-behavior(gen_event).

-include("log.hrl").

% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	code_change/3]).
% api
-export([start_link/0, set_channel/4, set_sup_channel/4, drop_channel/2]).
% non-public api
-export([handle_request/5, handle_response/5, handle_event/4]).

%% -------------------------------------------------------------------
%% api
%% -------------------------------------------------------------------

start_link() ->
	gen_event:start_link().

set_channel(Mgr, Channel, Module, Info) ->
	gen_event:add_handler(Mgr, {?MODULE, Channel}, {Channel, Module, Info}).

set_sup_channel(Mgr, Channel, Module, Info) ->
	gen_event:add_sup_handler(Mgr, {?MODULE, Channel}, {Channel, Module, Info}).

drop_channel(Mgr, Channel) ->
	gen_event:delete_handler(Mgr, {?MODULE, Channel}).

handle_request(Mgr, Client, Channel, Id, Request) ->
	gen_event:notify(Mgr, {request, Client, Channel, Id, Request}).

handle_response(Mgr, Client, Channel, Id, Response) ->
	gen_event:notify(Mgr, {response, Client, Channel, Id, Response}).

handle_event(Mgr, Client, Channel, Event) ->
	gen_event:notify(Mgr, {event, Client, Channel, Event}).


%% -------------------------------------------------------------------
%% gen_event
%% -------------------------------------------------------------------

init({Args, _}) ->
	init(Args);

init({_Channel, _Module, _Info} = State) ->
	{ok, State}.

%% -------------------------------------------------------------------

handle_event({request, Client, Channel, Id, Request} = Msg, {Channel, Module, Info} = State) ->
	Out = Module:client_request(Client, Id, Request, Info),
	handle_return(Out, Msg, State);

handle_event({response, Client, Channel, Id, Response} = Msg, {Channel, Module, Info} = State) ->
	Out = Module:client_response(Client, Id, Response, Info),
	handle_return(Out, Msg, State);

handle_event({event, Client, Channel, Event} = Msg, {Channel, Module, Info} = State) ->
	Out = Module:client_event(Client, Event, Info),
	handle_return(Out, Msg, State);

handle_event(_Msg, State) ->
	{ok, State}.

%% -------------------------------------------------------------------

handle_call(_Msg, State) ->
	{ok, invalid, State}.

%% -------------------------------------------------------------------

handle_info(_Msg, State) ->
	{ok, State}.

%% -------------------------------------------------------------------

terminate(_Msg, _State) ->
	ok.

%% -------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% -------------------------------------------------------------------
%% internal awesome
%% -------------------------------------------------------------------

handle_return(remove_handler, _Msg, _State) ->
	remove_handler;

handle_return({ok, NewChannelState}, _Msg, {Chan, Mod, _OldChannelState}) ->
	{ok, {Chan, Mod, NewChannelState}};

handle_return({ok, NewChannelState, hibernate}, _Msg, {Chan, Mod, _OldChannelState}) ->
	{ok, {Chan, Mod, NewChannelState}, hibernate};

handle_return({swap_handler, Args1, NewInfo, NewMod, NewChannelState}, _Msg, {Chan, Mod, _OldChannelState}) ->
	NewState = {Chan, Mod, NewInfo},
	{swap_handler, Args1, NewState, {?MODULE, Chan}, {Chan, NewMod, NewChannelState}};

handle_return({reply, Payload}, {request, Client, Channel, Id, _}, State) ->
	pre_client_connection:send_reply(Id, Payload),
	{ok, State}.
