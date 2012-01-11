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

start_link(Callback, Connection, Args) ->
	gen_server:start_link(?MODULE, {Callback, Connection, Args, []}, []).

behavior_info(callbacks) ->
	[{handle_client_event, 3},
	{handle_client_response, 3},
	{handle_client_request, 3},
	{handle_call, 3},
	{handle_cast, 2},
	{handle_info, 2},
	{teminate, 2},
	{code_change, 3}].

call(Pid, Req) ->
	call(Pid, Req, 5000).

call(Pid, Req, Timeout) ->
	gen_server:call(Pid, Req, Timeout).

cast(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

client_event(Pid, Channel, Event) when is_record(Event, event) ->
	gen_server:cast(Pid, {{'$pre_client', client_event}, {Channel, Event}}).

client_request(Pid, Channel, Request) when is_record(Request, request) ->
	gen_server:cast(Pid, {{'$pre_client', client_request}, {Channel, Request}}).

client_response(Pid, Channel, Response) when is_record(Response, response) ->
	gen_server:cast(Pid, {{'$pre_client', client_response}, {Channel, Response}}).

reply(Client, Reply) ->
	gen_server:reply(Client, Reply).

%% ==================================================================
%% gen_server Function Definitions
%% ==================================================================

%% ------------------------------------------------------------------
%% init
%% ------------------------------------------------------------------

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

handle_call(Req, From, State) ->
	#state{callback = Mod, substate = SubState} = State,
	ModResponse = Mod:handle_call(Req, From, SubState),
	handle_callback_return(ModResponse, State).

%% ------------------------------------------------------------------
%% handle_cast
%% ------------------------------------------------------------------

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

handle_info(Msg, State) ->
	#state{callback = Mod, substate= Substate} = State,
	ModResponse = Mod:handle_info(Msg, Substate),
	handle_callback_return(ModResponse, State).

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

terminate(Reason, State) ->
	#state{callback = Mod, substate = Sub} = State,
	Mod:terminate(Reason, Sub).

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

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
