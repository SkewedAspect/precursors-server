-module(post_chat_channeler).

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/pre_client.hrl").
% api
-export([start_link/0,client_login_hook/2]).
% channel stuf
-export([client_request/4,client_response/4,client_event/3]).
% supervisor stuff
-export([init/1]).

%% public api
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

client_login_hook(undefined, Client) ->
	supervisor:start_child(?MODULE, [Client#client_info.channel_manager]).

%% gen_server
init([]) ->
	process_flag(trap_exit, true),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]),
	Kid = {id, {pre_client_channels, set_sup_channel, [<<"chat">>, ?MODULE, undefined]}, transient, brutal_kill, worker, [?MODULE]},
	{ok, {{simple_one_for_one, 3, 5}, [Kid]}}.

%% client channels
client_request(_Client, _Id, _Request,undefined) ->
	{ok, undefined}.

client_response(_Client, _Id, _Response, undefined) ->
	{ok, undefined}.

client_event(_Client, _Event, undefined) ->
	{ok, undefined}.
