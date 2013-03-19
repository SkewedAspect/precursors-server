%%% @doc
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_entity_engine_sup).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([start_link/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	entity_mapping = dict:new()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init([]) ->
	State = #state{},
	{ok, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_call({get_entity, EntityID}, _From, State) ->
	EnginePid = dict:fetch(EntityID, State#state.entity_mapping),
	Resp = pre_entity_engine:get_entity(EnginePid, EntityID),
    {reply, Resp, State};


handle_call({get_best_engine, _Entity}, _From, State) ->
    {reply, invalid, State};


handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast({start_entity_engine, _Args}, State) ->
    {noreply, State};


handle_cast(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

