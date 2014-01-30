%%% @doc
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_entity_engine_sup).
-behaviour(gen_server).

-include_lib("pre_channel/include/pre_entity.hrl").
-include("supervisor.hrl").

% External API
-export([start_link/1, call_all/1, cast_all/1, broadcast_update/2, broadcast_updates/1, get_entity_engine/1,
	add_entity/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	engine_supervisor :: pid(),
	entity_mapping = dict:new()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% External API
%% --------------------------------------------------------------------------------------------------------------------

start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Calls all other entity engine supervisors with Request.
%%
%% This makes a call into the other entity engine supervisors, and returns the results, along with any nodes that didn't
%% respond.

-spec call_all(Request :: term()) ->
	{Replies :: [{Node :: atom(), Reply :: term()}], BadNodes :: [Node :: atom()]}.

call_all(Request) ->
	gen_server:multi_call(?MODULE, Request).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Casts the given Request to all entity engines.

-spec cast_all(Request :: term()) ->
	[ok | _].

cast_all(Request) ->
	[gen_server:cast(Pid, Request) || Pid <- pg2:get_members(entity_engines)].

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sends the given Message to all entity engines.

-spec send_all(Message :: term()) ->
	[ok | _].

send_all(Message) ->
	send_all(Message, entity_engines).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sends the given Message to all entity engines.

-spec send_all(Message :: term(), ProcessGroup :: pid()) ->
	[ok | _].

send_all(Message, ProcessGroup) ->
	[Pid ! {self(), Message} || Pid <- pg2:get_members(ProcessGroup)].

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sends an entity update to all other entity engines.
%%
%% Convience function for sending the given update message to all listening entity engines with `send_all`.
%% The message is a JSON structure indicating the portions of state that have changed.

-spec broadcast_update(EntityID::binary(), Update :: any()) ->
	ok.

broadcast_update(EntityID, Update) ->
	send_all({updates, [{EntityID, Update}]}, entity_updates),
	ok.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sends a list of entity updates to all other entity engines.
%%
%% Convience function for sending the given updates message to all listening entity engines with `send_all`.
%% The message is a JSON structure indicating the portions of state that have changed.

-spec broadcast_updates(Updates :: [{binary(), any()}]) ->
	ok.

broadcast_updates([]) ->
	ok;

broadcast_updates(Updates) ->
	send_all({updates, Updates}, entity_updates),
	ok.

%% --------------------------------------------------------------------------------------------------------------------

get_entity_engine(EntityID) ->
	gen_server:call(?MODULE, {get_entity_engine, EntityID}).

%% --------------------------------------------------------------------------------------------------------------------

-spec add_entity(Entity::#entity{}) ->
	ok.

add_entity(Entity) ->
	gen_server:cast(?MODULE, {add_entity, Entity}).

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

% Set up the supervisor.
?DYNAMIC_SUPERVISOR_INIT(?CHILD_GEN_SERVER(pre_entity_engine, [], transient));

init(Options) when is_list(Options) ->
	% Start entity engine supervisor.
	{ok, Supervisor} = supervisor:start_link({local, pre_entity_engine}, ?MODULE, supervisor),
	State = #state{
		engine_supervisor = Supervisor
	},

	% Start entity engine processes.
	InitialEngines = proplists:get_value(initial_engines, Options, 1),
	[start_engine(State) || _ <- lists:seq(1, InitialEngines)],

	{ok, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_call({get_entity_engine, EntityID}, _From, State) ->
	EnginePid = dict:fetch(EntityID, State#state.entity_mapping),
    {reply, EnginePid, State};

handle_call({get_entity, EntityID}, _From, State) ->
	EnginePid = dict:fetch(EntityID, State#state.entity_mapping),
	Resp = pre_entity_engine:get_entity(EnginePid, EntityID),
    {reply, Resp, State};

handle_call({add_to_local_engine, Entity}, _From, State) ->
	{Resp, State1} = add_to_local_engine(Entity, State),
    {reply, Resp, State1};

handle_call({move_to_local_engine, Entity}, _From, State) ->
	{Resp, State1} = move_to_local_engine(Entity, State),
    {reply, Resp, State1};

handle_call({entity_moved, EntityID, NewEnginePid}, _From, State) ->
	NewEntityMapping = dict:store(EntityID, NewEnginePid, State#state.entity_mapping),
	State1 = State#state{
		entity_mapping = NewEntityMapping
	},
    {reply, ok, State1};

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast({start_entity_engine, _Args}, State) ->
	start_engine(State),
    {noreply, State};

handle_cast({add_entity, Entity}, State) ->
	{_, NewState} = add_to_local_engine(Entity, State),
    {noreply, NewState};

handle_cast({entity_added, EntityID, EnginePid}, State) ->
	NewEntityMapping = dict:store(EntityID, EnginePid, State#state.entity_mapping),
	State1 = State#state{
		entity_mapping = NewEntityMapping
	},
    {noreply, State1};

handle_cast(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

terminate(Reason, _State) ->
	lager:info("Terminating due to ~p.", [Reason]),
	ok.

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal Helpers
%% --------------------------------------------------------------------------------------------------------------------

% Start a new entity engine process. (entity engines automatically join the 'entity_engines' process group, and can be
% accessed using the 'pg2' module)
start_engine(State) ->
	#state{
		engine_supervisor = Supervisor
	} = State,

	case supervisor:start_child(Supervisor, []) of
		{ok, _Pid} ->
			ok;
		Error ->
			lager:error("Invalid response from supervisor:start_child while starting entity engine: ~p", [Error])
	end.

%% --------------------------------------------------------------------------------------------------------------------

% Add the given entity to a local entity engine process.
% TODO: Possibly add some load balancing other than random chance
add_to_local_engine(Entity, State) ->
	case pg2:get_closest_pid(entity_engines) of
		{error, _} = Error ->
			{Error, State};
		EnginePid ->
			EntityID = Entity#entity.id,

			pre_entity_engine:add_entity(EnginePid, Entity),

			NewEntityMapping = dict:store(EntityID, EnginePid, State#state.entity_mapping),
			State1 = State#state{
				entity_mapping = NewEntityMapping
			},
			{{ok, EnginePid}, State1}
	end.

%% --------------------------------------------------------------------------------------------------------------------

% Move the given entity to a local entity engine process.
% TODO: Possibly add some load balancing other than random chance
move_to_local_engine(Entity, State) ->
	case pg2:get_closest_pid(entity_engines) of
		{error, _} = Error ->
			{Error, State};
		EnginePid ->
			EntityID = Entity#entity.id,

			pre_entity_engine:receive_entity(EnginePid, Entity),

			NewEntityMapping = dict:store(EntityID, EnginePid, State#state.entity_mapping),
			State1 = State#state{
				entity_mapping = NewEntityMapping
			},
			{{ok, EnginePid}, State1}
	end.