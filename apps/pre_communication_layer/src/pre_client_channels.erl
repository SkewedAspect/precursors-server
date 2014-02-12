%% @doc Holds the channels for a client connection.  Backed by gen_event.
%% Uses a simple callback model.
%%
%% Each client gets its own {@module}, but a channel can point to the
%% same gen_server, or even simply have an internally handled state for
%% each client.  An hook is fired when a client connects, so that is a
%% prime opportunity for a supervisor-style process to call
%% {@link set_channel/4}, or {@link set_sup_channel} for a supervised
%% handler.
%%
%% The module passed into {@link set_channel/4} or {@link set_sup_channel} must
%% implement 3 functions:  client_request/4, client_response/4, and
%% client_event/3.
%%
%% == client_request/4 ==
%% <pre>
%% Module:client_request( Client :: pid(), Id :: any(), Request :: any(),
%%     Info :: any()) -> callback_return() ).
%%
%%     Client :: process of the client connection.  Responses and events
%%         can be sent to this process directly.
%%     Id :: An opaque data type used for sending a response.
%%     Request :: The json request payload.
%%     Info :: The internal state of the callback channel.
%%         callback_return() can update this.
%% </pre>
%% When the client makes a request to the channel, this callback is used.
%% The callback can either have a response generated for it by sending
%% a specific callback_return(), or it can send a response on it's own
%% using the {@link pre_client_connect:send/5} function.
%%
%% == client_response/4 ==
%% <pre>
%% Module:client_response( Client :: pid(), Id :: any(), Response :: any(),
%%     Info :: any() ) -> callback_return() ).
%%
%%     Client :: process of the client connection.  New requests can be
%%         sent to the process directly.
%%     Id :: An opaque data type indication which request this response is
%%         for.  It is up to the callback module to keep track of the
%%         requests it has made.
%%     Response :: The json response payload.
%%     Info :: The internal state of the callback channel.
%%         callback_return() can update this.
%% </pre>
%% The client has sent a response to a request the channel made (or at
%% least it's claiming to).
%%
%% == client_event/3 ==
%% <pre>
%% Module:client_event( Client :: pid(), Event :: any(), Info() ) ->
%%     callback_return() ).
%%
%%     Client :: Process of the client connection.  New requests or events
%%         can be sent to the process directly.
%%     Event :: The payload for the event.
%%     Info :: The internal state of the callbakc channel.
%%         callback_return() can update this.
%% </pre>
%%
%% The client has sent an event to the channel.
%%
%% == callback_return() ==
%%
%% This can take many forms, many of which are the same as gen_event.
%% They are documented here for great good.
%%
%% === 'remove_handler' ===
%%
%% Remove the particular channel callback from the client's channels.
%%
%% === {'ok', NewChannelState :: any()} ===
%%
%% Updates the internal `Info' used by the callback functions.
%%
%% === {'ok', NewChannelState :: any(), 'hibernate'} ===
%%
%% Same as above, but puts the channel handler into hibernation.  This
%% indicates it will not be processing new events for at least 2 garbage
%% collection cycles.
%%
%% === {'swap_handler', Args1 :: any(), NewInfo :: any(), NewMod :: atom(), NewChannelState :: any()} ===
%%
%% Swaps the callback module for a new one.  Behaves the same as the
%% gen_event return of the same signature.
%%
%% === {'reply', Payload :: any()} ===
%%
%% Only valid when the callback function {@link client_request/4} was used.
%% Sends a response to the client using the same ID that came in over tcp.
%%
%% === {'reply', Socket :: 'tcp' | 'ssl', Payload :: any()} ===
%%
%% Same as above, but allows one to specific which socket to use.

-module(pre_client_channels).
-behaviour(gen_event).

-include("pre_client.hrl").

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

%% @doc Starts linked to the calling process.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	gen_event:start_link().

%% @doc Add a new channel to the manager.  Info is the initial state of
%% the channel.
-spec(set_channel/4 :: (Mgr :: pid(), Channel :: any(), Module :: atom(),
	Info :: any()) -> 'ok').
set_channel(Mgr, Channel, Module, Info) ->
	gen_event:add_handler(Mgr, {?MODULE, Channel}, {Channel, Module, Info}).

%% @doc Add a new channel to the manager, supervising the handler should
%% it horribly die.
-spec(set_sup_channel/4 :: (Mgr :: pid(), Channel :: any(),
	Module :: atom(), Info :: any()) -> 'ok').
set_sup_channel(Channel, Module, Info, Mgr) ->
	gen_event:add_sup_handler(Mgr, {?MODULE, Channel}, {Channel, Module, Info}).

%% @doc Remove the channel from the manager.
-spec(drop_channel/2 :: (Mgr :: pid(), Channel :: any()) -> any()).
drop_channel(Mgr, Channel) ->
	gen_event:delete_handler(Mgr, {?MODULE, Channel}).

%% @doc Used by the client connection to send requests into the manager.
%% Requests end up calling into client_request/4 of the callback module.
-spec(handle_request/5 :: (Mgr :: pid(), ClientInfo :: #client_info{}, Channel :: any(),
	Id :: any(), Request :: any()) -> 'ok').
handle_request(Mgr, ClientInfo, Channel, Id, Request) ->
	%?debug("Handling: Mgr=~p, ClientInfo=~p, Channel=~p, Id=~p, Request=~p",
	%	[Mgr, ClientInfo, Channel, Id, Request]),
	gen_event:notify(Mgr, {request, ClientInfo, Channel, Id, Request}).

%% @doc Used by the client connection to send responses into the manager.
%% Requests end up calling into client_response/4 of the callback module.
-spec(handle_response/5 :: (Mgr :: pid(), ClientInfo :: #client_info{},
	Channel :: any(), Id :: any(), Response :: any()) -> 'ok').
handle_response(Mgr, ClientInfo, Channel, Id, Response) ->
	gen_event:notify(Mgr, {response, ClientInfo, Channel, Id, Response}).

%% @doc Used by the client connection to send events into the manager.
%% Events end up calling into the client_event/3 of the callback module.
-spec(handle_event/4 :: (Mgr :: pid(), ClientInfo :: #client_info{}, Channel :: any(),
	Event :: any()) -> 'ok').
handle_event(Mgr, ClientInfo, Channel, Event) ->
	gen_event:notify(Mgr, {event, ClientInfo, Channel, Event}).


%% -------------------------------------------------------------------
%% gen_event
%% -------------------------------------------------------------------

%% @hidden
init({Args, _}) ->
	init(Args);

init({_Channel, _Module, _Info} = State) ->
	{ok, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_event({request, ClientInfo, Channel, Id, Request} = Msg, {Channel, Module, Info} = State) ->
	Out = Module:client_request(ClientInfo, Id, Request, Info),
	handle_return(Out, Msg, State);

handle_event({response, ClientInfo, Channel, Id, Response} = Msg, {Channel, Module, Info} = State) ->
	Out = Module:client_response(ClientInfo, Id, Response, Info),
	handle_return(Out, Msg, State);

handle_event({event, ClientInfo, Channel, Event} = Msg, {Channel, Module, Info} = State) ->
	Out = Module:client_event(ClientInfo, Event, Info),
	handle_return(Out, Msg, State);

handle_event(_Msg, State) ->
	{ok, State}.
%% -------------------------------------------------------------------

%% @hidden
handle_call(_Msg, State) ->
	{ok, invalid, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_info(_Msg, State) ->
	{ok, State}.

%% -------------------------------------------------------------------

%% @hidden
terminate(_Msg, _State) ->
	ok.

%% -------------------------------------------------------------------

%% @hidden
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

handle_return({reply, Payload}, Msg, State) ->
	handle_return({reply, tcp, Payload}, Msg, State);

handle_return({reply, Socket, Payload}, {request, Client, Channel, Id, _}, State) ->
	pre_client_connection:send(Client, Socket, {response, Id}, Channel, Payload),
	{ok, State}.
