-module(pre_client_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("log.hrl").

-record(state, {
	ets
}).
-record(client_connection, {
	pid,
	ssl_socket,
	tcp_socket,
	udp_socket
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,start_link/1,start/0,start/1,
	start_client/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() -> start_link([]).

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

start() -> start([]).

start(Args) ->
	gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

start_client(Socket) ->
	gen_server:call(?MODULE, {start_client, Socket}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	process_flag(trap_exit, true),
	Ets = ets:new(client_ets, [named_table, public, set, {keypos,2}]),
  {ok, #state{ets = Ets}}.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

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
			?notice("Could not start new client for socket ~p due to ~p", [Socket, Else]),
			{reply, Else, State}
	end;

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

%% ------------------------------------------------------------------
%% handle_cast
%% ------------------------------------------------------------------

handle_cast(_Msg, State) ->
  {noreply, State}.

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

handle_info({'EXIT', Pid, Cause}, State) ->
	?info("Handling an exit of ~p due to ~p", [Pid, Cause]),
	#state{ets = Ets} = State,
	ets:delete(Ets, Pid),
	{noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

%% ------------------------------------------------------------------
%% code_change
%% ------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

make_bin_ref() ->
	list_to_binary(io_lib:format("~p", [make_ref()])).
