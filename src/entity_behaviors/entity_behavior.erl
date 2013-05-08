%%% @doc The behavior for all of our entity behavior modules.
%%%
%%% `init(Entity)' is called when a new entity is created (or loaded from the database). It should setup the default
%%% state required by the behavior. It should return an updated Entity record.
%%%
%%% `simulate(Entity, EntityEngineState)' is called every simulation frame by the entity engine. It is expected
%%% to return `{Update, NewEntity}` where `Update` is either a JSON structure representing any changes or `undefined`
%%% if no changes occurred, and `NewEntity` is the new entity record that should be used for future simulation.
%%%
%%% `get_client_behavior()' is called to get the name of the client-side behavior that corresponds to this behavior
%%% module. It should return a binary..
%%%
%%% `get_full_state(Entity)' is called whenever a full JSON state message is required. It is expected to return
%%% a JSON structure representing the entity's current state.
%%%
%%% `client_request(Channel, RequestType, RequestID, Request, Entity)' is called by the entity/client communication
%%% interface (pre_entity_comm) when the client sends a request that the behavior needs to process. It is expected to
%%% return `{ok, Response, NewEntity}` where `Response` is a JSON structure to send back to the client.
%%%
%%% `client_event(ClientInfo, Channel, EventType, Event, Entity)' is called by the entity/client communication
%%% interface (pre_entity_comm) when the client sends an event that the behavior needs to process. No response is
%%% expected.
%%%
%%% `entity_event(Event, From, Entity)' is called by the entity engine whenever another entity sends or broadcasts an
%%% event. No response is expected.
%%% 
%%% `apply_update(Key, Value, Entity)' is called by the entity engine and its worker process in order to update the
%%% local entity state with the given update. No response is expected.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_behavior).

-include("log.hrl").
-include("pre_entity.hrl").

-export([behaviour_info/1]).

% API
-export([call/3, call/4, apply_updates/2]).

%% --------------------------------------------------------------------------------------------------------------------
%% Behaviour
%% --------------------------------------------------------------------------------------------------------------------

behaviour_info(callbacks) ->
	[{init, 1}, {simulate, 2}, {get_client_behavior, 0}, {get_full_state, 1}, {client_request, 5}, {client_event, 5},
		{entity_event, 3}, {apply_update, 3}];

behaviour_info(_) ->
    undefined.

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

call(Entity, Func, Args) ->
	call(Entity, Func, Args, fun handle_result/2).

call(Entity, Func, Args, HandleResult) ->
	#entity{
		id = EntityID,
		behavior = Behavior
	} = Entity,

	% Call the behavior
	try apply(Behavior, Func, Args) of
		Result ->
			HandleResult(Result, {Entity, Func, Args})
	catch
		Exception ->
			?error("Exception while calling behavior function ~p:~p(~p) for entity ~p: ~p",
				[Behavior, Func, Args, EntityID, Exception]),
			{noreply, Entity}
	end.

%% --------------------------------------------------------------------------------------------------------------------

apply_updates([], Entity) ->
	Entity;

apply_updates([{Key, Value} | Rest], Entity) ->
	{undefined, Entity1} = call(Entity, apply_update, [Key, Value, Entity], fun return_result/2),
	apply_updates(Rest, Entity1).

%% --------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------------------------------------------------------

-spec handle_result(BehaviorFuncResult, Context) -> GenServerResponse when
	BehaviorFuncResult :: term(),
	Context :: {OriginalEntity, Func, Args},
		OriginalEntity :: #entity{},
		Func :: atom(),
		Args :: list(),
	GenServerResponse :: {reply, Reply, NewEntity} | {noreply, NewEntity},
		Reply :: json(),
		NewEntity :: #entity{}.

% 3-tuples
handle_result({Reply, Update, #entity{} = NewEntity}, Ctx) ->
	handle_update(Update, Ctx),
	handle_reply(Reply, NewEntity, Ctx);

handle_result({_, _, UnrecognizedEnt}, {OriginalEntity, Func, Args}) ->
	Behavior = OriginalEntity#entity.behavior,
	?error("Unrecognized entity record in 3-tuple result from ~p:~p(~p): ~p", [Behavior, Func, Args, UnrecognizedEnt]),
	{noreply, OriginalEntity};

% 2-tuples
handle_result({Update, #entity{} = NewEntity}, Ctx) ->
	handle_update(Update, Ctx),

	{noreply, NewEntity};

handle_result({_, UnrecognizedEnt}, {OriginalEntity, Func, Args}) ->
	Behavior = OriginalEntity#entity.behavior,
	?error("Unrecognized entity record in 2-tuple result from ~p:~p(~p): ~p", [Behavior, Func, Args, UnrecognizedEnt]),
	{noreply, OriginalEntity};

% Other
handle_result(Unrecognized, {OriginalEntity, Func, Args}) ->
	Behavior = OriginalEntity#entity.behavior,
	?error("Unrecognized result from ~p:~p(~p): ~p", [Behavior, Func, Args, Unrecognized]),
	{noreply, OriginalEntity}.

%% --------------------------------------------------------------------------------------------------------------------

handle_update(undefined, _Ctx) -> ok;

handle_update([{_K, _V} | _] = Update, {OriginalEntity, _Func, _Args}) ->
	#entity{
		id = EntityID,
		client = ClientInfo
	} = OriginalEntity,
	send_update(ClientInfo, EntityID, Update);

handle_update([], {_OriginalEntity, _Func, _Args}) -> ok;

handle_update(UnrecognizedUpdate, {OriginalEntity, Func, Args}) ->
	Behavior = OriginalEntity#entity.behavior,
	?error("Unrecognized update in result from ~p:~p(~p): ~p", [Behavior, Func, Args, UnrecognizedUpdate]).

%% --------------------------------------------------------------------------------------------------------------------

handle_reply(undefined, NewEntity, _Ctx) ->
	{noreply, NewEntity};

handle_reply([{_K, _V} | _] = Reply, NewEntity, _Ctx) ->
	{reply, Reply, NewEntity};

handle_reply(UnrecognizedReply, NewEntity, {OriginalEntity, Func, Args}) ->
	Behavior = OriginalEntity#entity.behavior,
	?error("Unrecognized reply in result from ~p:~p(~p): ~p", [Behavior, Func, Args, UnrecognizedReply]),
	{noreply, NewEntity}.

%% --------------------------------------------------------------------------------------------------------------------

send_update(undefined, EntityID, Update) ->
	% Send the entity update to all other entity engines
	pre_entity_engine_sup:broadcast_update(EntityID, Update);

send_update(ClientInfo, EntityID, Update) ->
	% Send the entity update to this entity's client
	%?debug("Sending entity update for entity ~p to client ~p (self):~n~p", [EntityID, ClientInfo, Update]),
	pre_entity_comm:send_update(ClientInfo, EntityID, Update),
	send_update(undefined, EntityID, Update).

%% --------------------------------------------------------------------------------------------------------------------

return_result(Result, _Ctx) ->
	Result.
