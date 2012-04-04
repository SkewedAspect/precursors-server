-module(post_chat_channeler).

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/pre_client.hrl").
% api
-export([start_link/0,client_login_hook/2]).
% channel stuf
-export([client_request/4,client_response/4,client_response/4]).
% gen_server stuff
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
	code_change/3]).

%% public api
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

client_login_hook(undefined, Client) ->
	gen_server:cast(?MODULE, {set_chat_channel, Client}).

%% gen_server
init([]) ->
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]),
	
	{ok, undefined}.

handle_call(_Msg, _From, State) ->
	{reply, {error, nyi}, State}.

handle_cast({set_chat_channel, Client}, State) ->
	pre_client_channels:set_sup_channel(Client#client_info.channel_manager,
		<<"chat">>, ?MODULE, undefined),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_,_) ->
	ok.

code_change(_,State,_) ->
	{ok, State}.

%% client channels
client_request(_Client, _Id, _Request,undefined) ->
	{ok, undefined}.

client_response(_Client, _Id, _Response, undefined) ->
	{ok, undefined}.

client_event(_Client, _Event, undefined) ->
	{ok, undefined}.
