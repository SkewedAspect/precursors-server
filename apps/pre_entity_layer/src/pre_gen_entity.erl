-module(pre_gen_entity).

-callback init(Args :: list(any())) -> {'ok', any()}.
-callback handle_info(Name :: atom(), From :: 'undefined' | any(), To :: 'undefined' | any(), State :: any()) -> {'ok', any()}.

-behavior(gen_event).

% api
-export([
	add_entity/4
]).

% gen_event
-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	module, id, state
}).

% pulic api

add_entity(GenEventPid, EntityId, CallbackMod, Args) ->
	gen_event:add_handler(GenEventPid, {?MODULE, EntityId}, {EntityId, CallbackMod, Args}).

% gen_event callbacks

init({EntityId, Module, Args}) ->
	case Module:init(Args) of
		{ok, SubState} ->
			State = #state{module = Module, id = EntityId, state = SubState},
			{ok, State};
		Else ->
			Else
	end.

handle_event(_,_) -> ok.

handle_call(_,_) -> ok.

handle_info(_,_) -> ok.

terminate(_,_) -> ok.

code_change(_,_,_) -> ok.

