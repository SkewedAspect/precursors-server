-module(pre_client_listeners).

-behavior(supervisor).

-export([
	start_link/0,
	start_link/1,
	init/1
]).

start_link() -> start_link([]).

start_link(Args) ->
	ok.

% ===== init =====

init(Args) ->
	ok.
