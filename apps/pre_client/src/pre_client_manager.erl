%% @doc Watches client connections, murdering them if needed.
-module(pre_client_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("pre_channel/include/pre_client.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(state, {
	ets
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_link/1, start/0, start/1, start_client/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Starts using default options linked to calling process.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() -> start_link([]).

%% @doc Starts using the given args linked to the calling process.  There
%% are no options for the moment, just use {@link start_link/0} for now.
-spec(start_link/1 :: (Args :: any()) -> {'ok', pid()}).
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% @doc Starts unlinked, otherwise same as {@link start_link/0}.
-spec(start/0 :: () -> {'ok', pid()}).
start() -> start([]).

%% @doc Starts unlinked, otherwise same as {@link start_link/1}).
-spec(start/1 :: (Args :: any()) -> {'ok', pid()}).
start(Args) ->
	gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

%% @doc Spawn up a new client connection using the given socket as the ssl
%% socket.
-spec(start_client/1 :: (Socket :: any()) -> {'ok', pid()}).
start_client(Socket) ->
	gen_server:call(?MODULE, {start_client, Socket}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @hidden
init(_Args) ->
	process_flag(trap_exit, true),
	Ets = ets:new(client_ets, [named_table, public, set, {keypos,2}]),
	{ok, #state{ets = Ets}}.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @hidden
handle_call({start_client, Socket}, _From, State) ->
	Cookie = make_bin_ref(),
	case pre_client_connection:start_link(Socket, Cookie) of
		{ok, Pid} ->
			Client = #client_connection{
				pid = Pid,
				ssl_socket = Socket,
				tcp_socket = Cookie,
				udp_socket = Cookie
			},
			#state{ets = Ets} = State,
			ets:insert(Ets, [Client]),
			{reply, {ok, Pid}, State};
		Else ->
			lager:notice("Could not start new client for socket ~p due to ~p", [Socket, Else]),
			{reply, Else, State}
	end;

handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

%% ------------------------------------------------------------------
%% handle_cast
%% ------------------------------------------------------------------

%% @hidden
handle_cast(_Msg, State) ->
	{noreply, State}.

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

%% @hidden
handle_info({'EXIT', Pid, Cause}, State) ->
	lager:info("Handling an exit of ~p due to ~p", [Pid, Cause]),
	#state{ets = Ets} = State,
	ets:delete(Ets, Pid),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

%% @hidden
terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% code_change
%% ------------------------------------------------------------------

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

make_bin_ref() ->
	list_to_binary(io_lib:format("~p", [make_ref()])).
