%%% @doc Callbacks for the "entity" channel.

-module(pre_ping_channel).

-behaviour(pre_gen_channel).

-include("pre_client.hrl").

% API
-export([handle_request/4, handle_response/4, handle_event/3]).

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_request(<<"ping">>, ID, _Request, State) ->
	Timestamp = generate_timestamp(),

	% Build the ping response
	PingResponse = [
		{confirm, true},
		{timestamp, Timestamp}
	],

	% Send the ping response
	pre_client:send_response(self(), <<"ping">>, ID, PingResponse),
	State;

handle_request(Type, ID, Request, State) ->
	lager:warning("[Ping] Unknown Request: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_response(Type, ID, Request, State) ->
	lager:warning("[Ping] Unknown Response: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_event(Type, Request, State) ->
	lager:warning("[Ping] Unknown Event: ~p, ~p", [Type, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

%% This just generates a (floating-point) number representing a number of seconds.
generate_timestamp() ->
	generate_timestamp(os:timestamp()).


generate_timestamp({MegaSecs, Secs, MicroSecs}) ->
	MegaSecs * 1000000 + Secs + MicroSecs / 1000000.
