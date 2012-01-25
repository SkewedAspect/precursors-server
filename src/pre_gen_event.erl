-module(pre_gen_event).
-behavior(gen_event).

-include("log.hrl").

% api
-export([behavior_info/1]).
% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	code_change/3, format_status/2]).

-record(state, {
	callback,
	substate
}).

%% ==================================================================
%% API
%% ==================================================================

behavior_info(callbacks) ->
	[{client_event, 5},
	{client_response,5},
	{client_request,5},
	{handle_event,3},
	{handle_call,3},
	{handle_info,4},
	{terminate,2},
	{init,1},
	{code_change,3},
	{format_status,2}].

%% ==================================================================
%% gen_event callbacks
%% ==================================================================

%% ------------------------------------------------------------------
%% init
%% ------------------------------------------------------------------

init({Callback, Args}) ->
	case Callback:init(Args) of
		{ok, Sub} ->
			{ok, #state{callback = Callback, substate = Sub}};
		{ok, Sub, Hiber} ->
			{ok, #state{callback = Callback, substate = Sub}, Hiber};
		Else ->
			Else
	end.

%% ------------------------------------------------------------------
%% handle_event
%% ------------------------------------------------------------------

handle_event({{'$pre_client', EventHint}, {Channel, Conn, Client, Event}}, State) when is_atom(EventHint) ->
	#state{callback = Callback, substate = Sub} = State,
	case erlang:exported(Callback, EventHint, 4) of
		true ->
			Return = erlang:apply(Callback, EventHint, [Channel, Conn, Client, Event, Sub]),
			handle_return(Return, Conn, Client, State);
		false ->
			?warning("Could not handle hinted event:  ~s", [EventHint]),
			{ok, State}
	end;

handle_event(Event, State) ->
	#state{substate = Sub, callback = Callback} = State,
	Return = Callback:handle_event(Event, Sub),
	handle_return(Return, undefined, undefined, State).
	
%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

handle_call(Req, State) ->
	#state{callback = Callback, substate = Sub} = State,
	Return = Callback:handle_call(Req, undefined, Sub),
	handle_return(Return, undefined, undefined, State).

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

handle_info({{'$pre_client', info}, {Conn, Client, Msg}}, State) ->
	#state{callback = Callback, substate = Sub} = State,
	Return = Callback:handle_info(Msg, Conn, Client, Sub),
	handle_return(Return, Conn, Client, State);

handle_info(Info, State) ->
	#state{callback = Callback, substate = Sub} = State,
	Return = Callback:handle_info(Info, undefined, Sub),
	handle_return(Return, undefined, undefined, State).

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

terminate(Arg, State) ->
	#state{callback = Cb, substate = Sub} = State,
	Cb:terminate(Arg, Sub).

%% ------------------------------------------------------------------
%% code_change
%% ------------------------------------------------------------------

code_change(OldVsn, State, Extra) ->
	#state{callback = Cb, substate = Sub} = State,
	{ok, NewSub} = Cb:code_change(OldVsn, Sub, Extra),
	State0 = State#state{substate = NewSub},
	{ok, State0}.

%% ------------------------------------------------------------------
%% format_status
%% ------------------------------------------------------------------

format_status(Opt, [Pdict, State]) ->
	#state{callback = Cb, substate = Sub} = State,
	Status = Cb:format_status(Opt, [Pdict, Sub]),
	State#state{substate = Status}.

%% ==================================================================
%% Internal Funtions
%% ==================================================================

handle_return({ok, Sub}, _Conn, _Client, State) ->
	State0 = State#state{substate = Sub},
	{ok, State0};

handle_return({ok, Sub, hibernate}, _Conn, _Client, State) ->
	State0 = State#state{substate = Sub},
	{ok, State0, hibernate};

handle_return({swap_handler, Args1, NewSub, Handler2, Args2}, _Conn, _Client, State) ->
	State0 = State#state{substate = NewSub},
	{swap_handler, Args1, State0, {pre_gen_event, Handler2}, Args2};

handle_return(remove_handler, _Conn, _Client, _State) ->
	remove_handler;

handle_return({ok, Reply, Sub0}, _Conn, _Client, State) ->
	State0 = State#state{substate = Sub0},
	{ok, Reply, State0};

handle_return({ok, Reply, Sub0, hibernate}, _Conn, _Client, State) ->
	State0 = State#state{substate = Sub0},
	{ok, Reply, State0, hibernate};

handle_return({swap_handler, Reply, Args1, Sub0, Handler2, Args2}, _Conn, _Client, State) ->
	State0 = State#state{substate = Sub0},
	{swap_handler, Reply, Args1, State0, {pre_gen_event, Handler2}, Args2};

handle_return({remove_handler, Reply}, _Conn, _Client, _State) ->
	{remove_handler, Reply};

handle_return({send, Socket, Binary, Sub0}, undefined, _Client, State) ->
	State0 = State#state{substate = Sub0},
	?info("Trying to send to an unknown client", []),
	{ok, State0};

handle_return({send, Reply, Socket, Binary, Sub0}, undefined, _Client, State) ->
	State0 = State#state{substate = Sub0},
	?info("Trying to send to an unknown client", []),
	{ok, State0};

handle_return({send, Socket, Binary, Sub0}, Conn, _Client, State) ->
	State0 = State#state{substate = Sub0},
	pre_client_connection:send(Conn, Socket, Binary),
	{ok, State0};

handle_return({send, Reply, Socket, Binary, Sub0}, Conn, _Client, State) ->
	State0 = State#state{substate = Sub0},
	pre_client_connection:send(Conn, Socket, Binary),
	{ok, Reply, State0};

handle_return({add_handler, Handler, Args, Sub0}, _Conn, Client, State) ->
	State0 = State#state{substate = Sub0},
	pre_gen_client:async_add_handler(Client, Handler, Args),
	{ok, State0};

handle_return({add_handler, Reply, Handler, Args, Sub0}, _Conn, Client, State) ->
	State0= State#state{substate = Sub0},
	pre_gen_client:async_add_sup_handler(Client, Handler, Args),
	{ok, State0}.
