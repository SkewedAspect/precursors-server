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
-export([start_link/3, behavior_info/1, call/2, call/3, cast/2,
	client_event/3, client_request/3, client_response/3, reply/2]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	connection :: pid(),
	callback :: atom(),
	substate :: any()
}).

%% ==================================================================
%% API Function Definitions
%% ==================================================================

%% @doc Start linked to the calling process.  `Args' is used in
%% `Callback:init/1'.
-spec(start_link/3 :: (Callback :: atom(), Connection :: pid(),
	Args :: any()) -> {'ok', pid()}).
start_link(Callback, Connection, Args) ->
	gen_server:start_link(?MODULE, {Callback, Connection, Args, []}, []).

%% @private
behavior_info(callbacks) ->
	[{handle_client_event, 3},
	{handle_client_response, 3},
	{handle_client_request, 3},
	{handle_call, 3},
	{handle_cast, 2},
	{handle_info, 2},
	{teminate, 2},
	{code_change, 3}];
behavior_info(_) ->
	invalid.

%% @doc Same as {@link gen_server:call/2}, just here to make code prettier.
-spec(call/2 :: (Pid :: pid(), Req :: any()) -> any()).
call(Pid, Req) ->
	call(Pid, Req, 5000).

%% @doc Same as {@link gen_server:call/3}, just here to make code prettier.
-spec(call/3 :: (Pid :: pid(), Req :: any(),
	Timeout :: 'infinity' | non_neg_integer()) -> any()).
call(Pid, Req, Timeout) ->
	gen_server:call(Pid, Req, Timeout).

%% @doc Same as {@link gen_server:cast/2}, just here to make code prettier.
-spec(cast/2 :: (Pid :: pid(), Msg :: any()) -> any()).
cast(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

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

%% @doc Same as {@link gen_server:reply/2}, just here to make code prettier.
-spec(reply/2 :: (Client :: {pid(), reference()}, Reply :: any()) -> 'ok').
reply(Client, Reply) ->
	gen_server:reply(Client, Reply).

%% ==================================================================
%% gen_server Function Definitions
%% ==================================================================

%% ------------------------------------------------------------------
%% init
%% ------------------------------------------------------------------

%% @private
init({Callback, Connection, Args, _Options}) ->
	case Callback:init(Args) of
		ignore ->
			ignore;
		{stop, Reason} ->
			{stop, Reason};
		{ok, Substate} ->
			State = #state{
				callback = Callback,
				substate = Substate,
				connection = Connection
			},
			{ok, State}
	end.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @private
handle_call(Req, From, State) ->
	#state{callback = Mod, substate = SubState} = State,
	ModResponse = Mod:handle_call(Req, From, SubState),
	handle_callback_return(ModResponse, State).

%% ------------------------------------------------------------------
%% handle_cast
%% ------------------------------------------------------------------

%% @private
handle_cast({{'$pre_client', client_event}, {Channel, Event}}, State) ->
	#state{callback = Mod, substate = SubState} = State,
	ModResponse = Mod:handle_client_event(Channel, Event, SubState),
	handle_callback_return(ModResponse, State);

handle_cast({{'$pre_client', client_request}, {Channel, Request}}, State) ->
	#state{callback = Mod, substate = Substate} = State,
	ModResponse = Mod:handle_client_request(Channel, Request, Substate),
	handle_callback_return(ModResponse, State);

handle_cast({{'$pre_client', client_response}, {Channel, Response}}, State) ->
	#state{callback = Mod, substate = Substate} = State,
	ModResponse = Mod:handle_client_response(Channel, Response, Substate),
	handle_callback_return(ModResponse, State);

handle_cast(Msg, State) ->
	#state{callback = Mod, substate = Substate} = State,
	ModResponse = Mod:handle_cast(Msg, Substate),
	handle_callback_return(ModResponse, State).

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @private
handle_info(Msg, State) ->
	#state{callback = Mod, substate= Substate} = State,
	ModResponse = Mod:handle_info(Msg, Substate),
	handle_callback_return(ModResponse, State).

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @private
terminate(Reason, State) ->
	#state{callback = Mod, substate = Sub} = State,
	Mod:terminate(Reason, Sub).

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @private
code_change(OldVsn, State, Extra) ->
	#state{callback = Mod, substate = Sub} = State,
	{ok, NewSub} = Mod:code_change(OldVsn, Sub, Extra),
	State0 = State#state{substate = NewSub},
	{ok, State0}.

%% ==================================================================
%% Internal Function Definitions
%% ==================================================================

%% ------------------------------------------------------------------
%% handle_callback_return
%% ------------------------------------------------------------------

handle_callback_return({mutate, Reply, NewMod, NewArgs}, State) ->
	case handle_mutate(NewMod, NewArgs) of
		{error, Err} -> {stop, Err, Reply, State};
		{ok, NewSub} ->
			{reply, Reply, State#state{callback = NewMod, substate = NewSub}}
	end;

handle_callback_return({mutate, NewMod, NewArgs}, State) ->
	case handle_mutate(NewMod, NewArgs) of
		{error, Err} -> {stop, Err, State};
		{ok, NewSub} ->
			{noreply, State#state{callback = NewMod, substate = NewSub}}
	end;

handle_callback_return({send, Reply, Sends, NewSub}, State) ->
	{noreply, State0} = handle_callback_return({send, Sends, NewSub}, State),
	{reply, Reply, State0};

handle_callback_return({send, [], NewSub}, State) ->
	State0 = State#state{substate = NewSub},
	{noreply, State0};

handle_callback_return({send, [{Sock, Bin}|Tail], NewSub}, State) ->
	#state{connection = Conn} = State,
	pre_client_connection:send(Conn, Sock, Bin),
	handle_callback_return({send, Tail, NewSub}, State).

%% ------------------------------------------------------------------

handle_mutate(NewMod, NewArgs) ->
	case NewMod:init(NewArgs) of
		{stop, Reason} -> {error, Reason};
		ignore -> {error, bad_mutate};
		{ok, _} = Ok -> Ok
	end.
