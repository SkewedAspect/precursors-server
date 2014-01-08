%%% @doc Abstraction layer for accessing persistant storage. Called
%%% cold-storage by the gang. Also defines the behavior of the callback
%%% modules.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_data).
-behaviour(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

-type error_return() :: {'error', any()}.
-type comparison_op() :: '>' | '>=' | '<' | '=<' | '==' | '=:='.
-type search_parameter() :: {any(), any()} | {any(), comparison_op(), any()}.
% behavior definition
-callback get_by_id(Type :: atom(), Id :: any()) -> {'ok', tuple()} | {'error', notfound} | error_return().
-callback save(Record :: tuple()) -> {'ok', tuple()} | error_return().
-callback delete(Type :: atom(), Id :: any()) -> 'ok' | error_return().
-callback search(Type :: atom(), Params :: [search_parameter()]) -> {'ok', []}.
-callback transaction(Fun :: fun(() -> any())) -> any().

% API
-export([start_link/1, stop/0]).
-export([get_by_id/2, save/1, delete/1, delete/2]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Start linked to the calling process with the given callback module.
-spec start_link(CallbackModule :: atom()) -> {'ok', pid()}.
start_link(CallbackModule) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, CallbackModule, []).

%% @doc Stops the server with reason normal.
-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?MODULE, stop).

%% @doc Attempts to get a record of the given type with the given id. This
%% expects to be in the context of a transaction, otherwise explosions may
%% occur.
-spec get_by_id(Record :: atom(), Id :: any()) -> {'ok', tuple()} | {'error', 'notfound'}.
get_by_id(Record, Id) ->
     gen_server:call(?MODULE, {api, get_by_id, [Record, Id]}, infinity).

%% @doc Stores the record to long term. Expected to be called in the
%% context of a transaction. If the id is `undefined' it is automatically
%% set.
-spec save(Record :: tuple()) -> {'ok', tuple()}.
save(Record) ->
    gen_server:call(?MODULE, {api, save, [Record]}, infinity).

%% @doc Extract the type and id of the record and call {@link delete/2}.
-spec delete(Record :: tuple()) -> 'ok'.
delete(Record) ->
    Type = element(1, Record),
    Id = element(2, Record),
    delete(Type, Id).

%% @doc Delete the record of the given type with the given Id. Expected to
%% be called within the context of a transaction.
-spec delete(Record :: atom(), Id :: any()) -> 'ok'.
delete(Record, Id) ->
    gen_server:call(?MODULE, {api, delete, [Record, Id]}, infinity).

%% @doc Search the data backend for records of the given types with the
%% given properties. Expected to be called within the context of a
%% transaction.
-spec search(Record :: atom(), Params :: [search_parameter()]) -> {'ok', [tuple()]} | {'error', any()}.
search(Record, Params) ->
    check_params(Params),
    gen_server:call(?MODULE, {api, search, [Record, Params]}, infinity).
	
%% @doc Runs the fun as a transaction. This allows save/1, get_by_id/2,
%% delete/1,2 to be used such that the underlying data system can rollback
%% the changes if any later action fails. There is no acutal guarentee the
%% underlying data system supports transactions.
-spec transaction(TransFun :: fun()) -> any().
transaction(Fun) ->
    gen_server:call(?MODULE, {api, transaction, [Fun]}, infinity).

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init(CallbackModule) ->
    {ok, CallbackModule}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private
handle_call({api, Function, Args}, From, CallbackModule) ->
    spawn(fun() ->
        Res = erlang:apply(CallbackModule, Function, Args),
        gen_server:reply(From, Res)
    end),
    {noreply, CallbackModule};

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast(stop, State) ->
    {stop, normal, State};

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

%% --------------------------------------------------------------------------------------------------------------------

check_params([]) ->
    ok;

check_params([{_Key, _Value} | Tail]) ->
    check_params(Tail);

check_params([{_Key, member, Value} | Tail]) when is_list(Value) ->
    check_params(Tail);

check_params([{_Key, Op, _Value} | Tail]) ->
    ValidOps = ['>', '>=', '<', '=<', '==', '=:='],
	  case lists:member(Op, ValidOps) of
		    false ->
				    throw({badarg, Op});
				true ->
				    check_params(Tail)
		end.

