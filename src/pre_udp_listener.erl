%% @doc A simple listener for upd.  Forwards packets to client manager,
%% which sends them onwards to the appropriate client.
-module(pre_udp_listener).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {
	socket
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0,start/1,start_link/0,start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	start_link([]).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

start() ->
	start([]).

start(Args) ->
	gen_server:start(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% init
%% ------------------------------------------------------------------

init(Args) ->
	Port = proplists:get_value(port, Args, 1338),
	case gen_udp:open(Port, [{active, once}]) of
		{ok, Socket} ->
			{ok, #state{socket = Socket}};
		Else ->
			{error, Else}
	end.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

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

handle_info({udp, Socket, _Ip, _InPort, _Packet} = Msg, #state{socket = Socket} = State) ->
	pre_client_manager:upd(Msg),
	{noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% ------------------------------------------------------------------
%% terminataion
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

