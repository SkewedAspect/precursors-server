%% @doc Extension to gen_server to fit the needs of the client_connection
%% better.  In addition to the usual gen_server callbacks, there are 3
%% new ones for handling messages coming from the client:
%% {@link client_event/3}, {@link client_request/3}, and
%% {@link client_response/3}.
%% 
%% pre_gent_client also adds 4 new return terms in addition to the standard
%% gen_server ones.  More exactly, there are 2 variations each on 2 new
%% ones.
%% ```
%% {mutate, NewMod, NewArgs}
%% {mutate, Reply, NewMod, NewArgs}
%% NewMod :: atom()
%% Reply, NewArgs :: any()'''
%% 
%% Replaces the orignal callback module with a new one.  `NewMod:init/1' is
%% called with `NewArgs'.  If `{ok, State}' is returned, processing 
%% continues with `State'.  Otherwise, the client stops.
%% ```
%% {send, SendMsgs, State}
%% {send, Reply, SendMsgs, State}
%% Reply, State :: any()
%% SendMsgs :: [{SocketHint, binary()}]
%% 	SocketHing :: tcp | ssl | udp'''
%%
%% Attempts to send the listed binaries through the client connection using
%% the hinted socket.  A pure fire and forget system.

-module(pre_gen_client).
-behaviour(gen_server).

-include("log.hrl").
-include("precursors_pb.hrl").

% API
-export([start_link/1, client_event/3, client_request/3, client_response/3,
	add_handler/3, add_sup_handler/3, notify/2, sync_notify/2, call/2,
	call/3, delete_handler/3, swap_handler/3, swap_sup_handler/3,
	which_handlers/1
]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	connection :: pid(),
	gen_event_mgr :: pid()
}).

%% ==================================================================
%% API Function Definitions
%% ==================================================================

%% @doc Start linked to the calling process.  `Args' is used in
%% `Callback:init/1'.
-spec(start_link/1 :: (Connection :: pid()) -> {'ok', pid()}).
start_link(Connection) ->
	gen_server:start_link(?MODULE, Connection, []).

%% @doc Asynchronously handle an event sent from the client to the server
%% (us).  Replies are usually sent through the `send' return structure.
-spec(client_event/3 :: (Pid :: pid(), Channel :: string(),
	Event :: #event{}) -> 'ok').
client_event(Pid, Channel, Event) when is_record(Event, event) ->
	gen_server:cast(Pid, {{'$pre_client', client_event}, {Channel, Event}}).

%% @doc Asynchonrously handle a request from the client to the server (us).
%% Replies are usually sent through `send' return structure.
-spec(client_request/3 :: (Pid :: pid(), Channel :: string(), 
	Request :: #request{}) -> 'ok').
client_request(Pid, Channel, Request) when is_record(Request, request) ->
	gen_server:cast(Pid, {{'$pre_client', client_request}, {Channel, Request}}).

%% @doc Asynchronously handle a response the client returned to a request
%% the server (us) sent.
-spec(client_response/3 :: (Pid :: pid(), Channel :: string(),
	Response :: binary()) -> 'ok').
client_response(Pid, Channel, Response) when is_record(Response, response) ->
	gen_server:cast(Pid, {{'$pre_client', client_response}, {Channel, Response}}).

add_handler(Client, Handler, Args) ->
	gen_server:call(Client, {{'$pre_client', add_handler}, {Handler, Args}}).

add_sup_handler(Client, Handler, Args) ->
	gen_server:call(Client, {{'$pre_client', add_sup_handler}, {Handler, Args}}).

async_add_handler(Client, Handler, Args) ->
	gen_server:cast(Client, {{'$pre_client', add_handler}, {Handler, Args}}).

async_add_sup_handler(Client, Handler, Args) ->
	gen_server:cast(Client, {{'$pre_client', add_sup_handler}, {Handler, Args}}).

notify(Client, Event) ->
	gen_server:cast(Client, {{'$pre_client', notify}, Event}).

sync_notify(Client, Event) ->
	gen_server:call(Client, {{'$pre_client', sync_notify}, Event}).

call(Client, Request) ->
	call(Client, Request, infinity).

call(Client, Request, Timeout) ->
	gen_server:call(Client, {{'$pre_client', call}, {Request, Timeout}}).

delete_handler(Client, Handler, Args) ->
	gen_server:call(Client, {{'$pre_client', delete_handler}, {Handler, Args}}).

swap_handler(Client, {Handler1, Args1}, {Handler2, Args2}) ->
	gen_server:call(Client, {{'$pre_client', swap_handler}, {{Handler1, Args1}, {Handler2, Args2}}}).

swap_sup_handler(Client, {Handler1, Args1}, {Handler2, Args2}) ->
	gen_server:call(Client, {{'$pre_client', swap_sup_handler}, {{Handler1, Args1}, {Handler2, Args2}}}).

which_handlers(Client) ->
	List = gen_server:call(Client, {{'$pre_client', which_handlers}, undefined}),
	[X || {pre_gen_event, X} <- List].

%% ==================================================================
%% gen_server Function Definitions
%% ==================================================================

%% ------------------------------------------------------------------
%% init
%% ------------------------------------------------------------------

%% @private
init(Connection) ->
	{ok, Eventer} = gen_event:start_link(),
	State = #state{
		connection = Connection,
		gen_event_mgr = Eventer
	},
	{ok, State}.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @private
handle_call({{'$pre_client', add_handler}, {Handler, Args}}, _From, State) ->
	#state{gen_event_mgr = Eventer} = State,
	Callback = case handler of
		{C, _} -> C;
		A when is_atom(A) -> A
	end,
	Out = gen_event:add_handler(Eventer, {pre_gen_event, Handler}, {Callback, Args}),
	{reply, Out, State};

handle_call({{'$pre_client', add_sup_handler}, {Handler, Args}}, _From, State) ->
	#state{gen_event_mgr = Eventer} = State,
	Callback = case Handler of
		{C, _} -> C;
		A when is_atom(A) -> A
	end,
	Out = gen_event:add_handler(Eventer, {pre_gen_event, Handler}, {Callback, Args}),
	{reply, Out, State};

handle_call({{'$pre_client', sync_notify}, Event}, _From, State) ->
	#state{gen_event_mgr = Eventer, connection = Conn} = State,
	Event0 = {{'$pre_client', notify}, {Conn, self(), Event}},
	Out = gen_event:sync_notify(Eventer, Event0),
	{reply, Out, State};

handle_call({{'$pre_client', call}, {Handler, Request, Timeout}}, _From, State) ->
	#state{connection = Conn, gen_event_mgr = Mgr} = State,
	Call0 = {{'$pre_client', call}, {Conn, self(), Request}},
	Out = gen_event:call(Mgr, {pre_gen_event, Handler}, Call0, Timeout),
	{reply, Out, State};

handle_call({{'$pre_client', delete_handler}, {Handler, Args}}, _From, State) ->
	#state{gen_event_mgr = Mgr, connection = Conn} = State,
	Out = gen_event:delete_handler(Mgr, {pre_gen_event, Handler}, Args),
	{reply, Out, State};

handle_call({{'$pre_client', swap_handler}, {{Handler1, Args1}, {Handler2, Args2}}}, _From, State) ->
	#state{connection = Conn, gen_event_mgr = Mgr} = State,
	Callback2 = case Handler2 of
		{C, _} -> C;
		A when is_atom(A) -> A
	end,
	RealHandler2 = {pre_gen_event, Handler2},
	RealArgs2 = {Callback2, Args2},
	RealHandler1 = {pre_gen_event, Handler1},
	Out = gen_event:swap_handler(Mgr, {RealHandler1, Args1}, {RealHandler2, RealArgs2}),
	{reply, Out, State};

handle_call({{'$pre_client', swap_sup_handler}, {{Handler1, Args1}, {Handler2, Args2}}}, _From, State) ->
	#state{connection = Conn, gen_event_mgr = Mgr} = State,
	Callback2 = case Handler2 of
		{C, _} -> C;
		A when is_atom(A) -> A
	end,
	RealHandler2 = {pre_gen_event, Handler2},
	RealArgs2 = {Callback2, Args2},
	RealHandler1 = {pre_gen_event, Handler1},
	Out = gen_event:swap_sup_handler(Mgr, {RealHandler1, Args1}, {RealHandler2, RealArgs2}),
	{reply, Out, State};

handle_call({{'$pre_client', which_handlers}, _}, _From, State) ->
	#state{gen_event_mgr = Mgr} = State,
	Handlers = gen_event:which_handlers(Mgr),
	{reply, Handlers, State};

handle_call(Req, _From, State) ->
	{reply, invalid, State}.

%% ------------------------------------------------------------------
%% handle_cast
%% ------------------------------------------------------------------

%% @private
handle_cast({{'$pre_client', client_event} = Tag, {Channel, Event} = Args}, State) when is_record(Event, event) ->
	handle_client_msg(Tag, Args, State),
	{noreply, State};

handle_cast({{'$pre_client', client_request} = Tag, {Channel, Request} = Args}, State) when is_record(Request, request) ->
	handle_client_msg(Tag, Args, State),
	{noreply, State};

handle_cast({{'$pre_client', client_response} = Tag, {Channel, Response} = Args}, State) when is_record(Response, response) ->
	handle_client_msg(Tag, Args, State),
	{noreply, State};

handle_cast({{'$pre_client', add_handler}, {Handler, Args}} = Msg, State) ->
	{reply, Out, State0} = handle_call(Msg, "self", State),
	?info("addhandler async got ~p", [Out]),
	{noreply, State0};

handle_cast({{'$pre_client', add_sup_handler}, {Handler, Args}} = Msg, State) ->
	{reply, Out, State0} = handle_call(Msg, "self", State),
	?info("add sup handler async got ~p", [Out]),
	{noreply, State0};

handle_cast(Msg, State) ->
	{noreply, State}.

%% ------------------------------------------------------------------

handle_client_msg(Tag, {Chan, Arg}, State) ->
	#state{connection = Conn, gen_event_mgr = Mgr} = State,
	gen_event:notify(Mgr, {Tag, {Chan, Conn, self(), Arg}}).

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

%% @private
handle_info(Msg, State) ->
	% well, maybe it was meant for the event manager.
	#state{connection = Conn, gen_event_mgr = Mgr} = State,
	Msg0 = {{'$pre_client', info}, {Conn, self(), Msg}},
	Mgr ! Msg0,
	{noreply, State}.

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

%% @private
terminate(Reason, State) ->
	#state{gen_event_mgr = Mgr} = State,
	Event = {{'$pre_client', terminate}, Reason},
	gen_event:notify(Mgr, Event).

%% ------------------------------------------------------------------
%% code_change
%% ------------------------------------------------------------------

%% @private
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% ==================================================================
%% Internal Function Definitions
%% ==================================================================
