%%% @doc The game state engine - handles syncing of game state to the database and between nodes

-module(pre_data).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([start_link/1]).
-export([get/2, get_cache/2, get_checked/2]).
-export([set/3, set_cache/3, set_cache/4, set_checked/3]).
-export([delete/2, delete_cache/2, delete_cache/3, delete_checked/2]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Default Riak connection parameters:
-define(DEFAULT_RIAK_PB_PORT, 8087).

-record(state, {
	host = "localhost",
	port = ?DEFAULT_RIAK_PB_PORT :: riakc_pb_socket:portnum(),
	riak_conn :: pid()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Gets the contents of Bucket/Key.
%%
%% This returns the value from the local ETS cache, or queries Riak and caches the value if the value was not already
%% in the cache.
-spec get(Bucket::binary(), Key::binary()) ->
    {ok, Value :: json()} | {error, Msg :: list()}.

get(Bucket, Key) ->
	case get_cache(Bucket, Key) of
		not_found ->
			% Cache miss; look it up in Riak.
			case get_checked(Bucket, Key) of
				{ok, Value} ->
					set_cache(Bucket, Key, Value);
				Error ->
					?error("Unexpected response when setting cache during cache miss. Response: ~p", [Error]),
					{error, "Failed to set cache with Riak's response."}
			end;
		Resp ->
			% We don't care about the return; get_cache returns sane values to the user, not us.
			Resp
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Gets the contents of Bucket/Key from the local cache.
%%
%% This returns the value from the local ETS cache, returning 'not_found' if it was not found.
-spec get_cache(Bucket::binary(), Key::binary()) ->
    {ok, Value :: json()} | {error, Msg :: list()} | not_found.

get_cache(Bucket, Key) ->
	case ets:lookup(?MODULE, {Bucket, Key}) of
		[] ->
			not_found;
		[Value] ->
			Value;
		Else ->
			?error("Unexpected response from ETS. Response was: ~p", [Else]),
			{error, "Error querying ETS cache."}
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Gets the contents of Bucket/Key from Riak, skipping the cache.
%%
%% This queries Riak for the given value, and caches the value in ETS.
-spec get_checked(Bucket::binary(), Key::binary()) ->
    {ok, Value :: json()} | {error, Msg :: list()}.

get_checked(Bucket, Key) ->
	gen_server:call(?MODULE, {get, Bucket, Key}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sets the contents of Bucket/Key to Value.
%%
%% This sets the value in the ETS cache, sends an update to the peer servers, and sets the value in Riak.
-spec set(Bucket::binary(), Key::binary(), Value::json()) ->
    ok | {error, Msg :: string()}.

set(Bucket, Key, Value) ->
	case set_cache(Bucket, Key, Value) of
		ok ->
			% Set Value in Riak.
			gen_server:call(?MODULE, {set, Bucket, Key, Value});
		Resp ->
			Resp
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sets the contents of Bucket/Key to Value in the local cache.
%%
%% This sets the value in the ETS cache and sends an update to the peer servers, but skips Riak.
-spec set_cache(Bucket::binary(), Key::binary(), Value::json()) ->
    ok | {error, Msg :: string()}.

set_cache(Bucket, Key, Value) ->
	set_cache(Bucket, Key, Value, true).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sets the contents of Bucket/Key to Value in the local cache.
%%
%% This sets the value in the ETS cache and sends an update to the peer servers, but skips Riak.
-spec set_cache(Bucket::binary(), Key::binary(), Value::json(), NotifyPeers :: boolean()) ->
    ok | {error, Msg :: string()}.

set_cache(Bucket, Key, Value, NotifyPeers) ->
	ets:insert(?MODULE, {{Bucket, Key}, Value}),
	case NotifyPeers of
		true ->
			%TODO: Notify peer servers.
			ok;
		_ ->
			ok
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sets the contents of Bucket/Key to Value, checking to see whether the write succeeds and whether a sibling was
%% created.
%%
%% This sets the value in Riak, and if no siblings were created, sets the value in the ETS cache and sends an update to
%% the peer servers.
-spec set_checked(Bucket::binary(), Key::binary(), Value::json()) ->
	ok | {siblings, SiblingValues :: [json()]} | {error, Msg :: string()}.

set_checked(Bucket, Key, Value) ->
	% Set the value in Riak, and check the results
	case gen_server:call(?MODULE, {set, Bucket, Key, Value}) of
		ok ->
			set_cache(Bucket, Key, Value);
		Resp ->
			Resp
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Deletes the contents of Bucket/Key.
%%
%% This deletes the value in the ETS cache, sends a delete to the peer servers, and deletes the value in Riak.
-spec delete(Bucket::binary(), Key::binary()) ->
    ok | {error, Msg :: string()}.

delete(Bucket, Key) ->
	case delete_cache(Bucket, Key) of
		ok ->
			% Delete from Riak
			gen_server:call(?MODULE, {delete, Bucket, Key});
		Resp ->
			Resp
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Deletes the contents of Bucket/Key from the local cache.
%%
%% This deletes the value from the local ETS cache and sends a delete to the peer servers, but skips Riak.
-spec delete_cache(Bucket :: binary(), Key :: binary()) ->
    ok | {error, Msg :: string()}.

delete_cache(Bucket, Key) ->
	delete_cache(Bucket, Key, true).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Deletes the contents of Bucket/Key from the local cache.
%%
%% This deletes the value from the local ETS cache and sends a delete to the peer servers, but skips Riak.
-spec delete_cache(Bucket :: binary(), Key :: binary(), NotifyPeers :: boolean()) ->
    ok | {error, Msg :: string()}.

delete_cache(Bucket, Key, NotifyPeers) ->
	case ets:delete(?MODULE, {Bucket, Key}) of
		true ->
			ok;
		Resp ->
			?error("Unexpected response while deleting object. Response: ~p", [Resp]),
			{error, "Failed to delete object from ETS."}
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Deletes the contents of Bucket/Key, checking to see whether the delete succeeds.
%%
%% This attempts to delete the value from the ETS cache and from Riak; if it succeeds, it sends a delete to the peer
%% servers.
-spec delete_checked(Bucket::binary(), Key::binary()) ->
	ok | {newer_version, NewVersion :: json()} | {error, Msg::list()}.

delete_checked(Bucket, Key) ->
	gen_server:call(?MODULE, {Bucket, Key}).

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init([Host, Port]) ->
	ets:new(?MODULE, [set, public, named_table]),
	?info("Host: ~p, Port: ~p", [Host, Port]),

	% Connect to riak
	{ok, RiakConn} = riakc_pb_socket:start_link(Host, Port),
	State = #state{
		host = Host,
		port = Port,
		riak_conn = RiakConn
	},
	{ok, State};

init([]) ->
	ets:new(?MODULE, [set, public, named_table]),
	?info("Here, instead.", []),

	% Pull state variables
	State = #state{},
	Host = State#state.host,
	Port = State#state.port,

	% Connect to riak
	{ok, RiakConn} = riakc_pb_socket:start_link(Host, Port),
	NewState = State#state{
		riak_conn = RiakConn
	},
	{ok, NewState}.


%% --------------------------------------------------------------------------------------------------------------------

handle_call({get, Bucket, Key}, _From, State) ->
	RiakConn = State#state.riak_conn,
	Resp = case riakc_pb_socket:get(RiakConn, Bucket, Key) of
		{ok, RiakCObj} ->
			JSON = riakc_obj:get_value(RiakCObj),
			Value = parse_json(JSON),
			{ok, Value};
		Error ->
			%TODO: Check for siblings... and do something?
			?error("Unexpected response from Riak. Response was: ~p", [Error]),
			{error, "Error querying ETS cache."}
	end,
	{reply, Resp, State};

handle_call({set, Bucket, Key, Value}, _From, State) ->
	{reply, {error, 'FECK'}, State};

handle_call({delete, Bucket, Key}, _From, State) ->
	{reply, {error, 'FECK'}, State};

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

parse_json(Bin) ->
	{not_really_json}.

%% --------------------------------------------------------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.
