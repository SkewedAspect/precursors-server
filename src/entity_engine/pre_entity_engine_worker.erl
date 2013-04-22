%%% @doc The entity engine's simulation worker process.
%%%
%%% This module has exactly one purpose: it runs entity simulations. That is all that it does. It is written as a pure
%%% erlang module, not an OTP module, because of issues encountered with running gen_server based modules at a fixed
%%% rate. There is an exact one to one relationship between entity engine workers, and entity engines. The worker also
%%% has it's own copy of the entity engine's state. This is stored as a proplist, rather than as as dict, for
%%% performance reasons.
%%%
%%% The entire reason for this extra process is to move the load of simulation to a different process from the one that
%%% has to reply to updates, or communicate with the rest of the system. This process can run at full speed, only
%%% responding to, or generating updates. The rest is handled by the entity engine proper.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_entity_engine_worker).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([start_worker/1]).

% Simulation interval
-define(INTERVAL, 33). % about 1/60th of a second. (16 ms)

%% --------------------------------------------------------------------------------------------------------------------
%% Worker API
%% --------------------------------------------------------------------------------------------------------------------

start_worker(InitialState) ->
  init(InitialState).

%% --------------------------------------------------------------------------------------------------------------------

init(InitialState) ->
  % Start the simulate timer
  erlang:send_after(?INTERVAL, self(), simulate),

  % Start the receive loop.
  do_receive(InitialState).

%% --------------------------------------------------------------------------------------------------------------------

do_receive(State) ->

  % Listen for messages being sent to the process.
  receive
    {From, Msg} ->
      {Reply, NewState} = handle_msg(Msg, From, State)
  end,

  % Handle potential replies
  case Reply of
    noreply ->
      ok;
    _ ->
      From ! Reply
  end,

  % Time to call do_simulate again.
  do_receive(NewState).

%% --------------------------------------------------------------------------------------------------------------------

handle_msg(simulate, _From, State) ->
  % Would do simulate here.

  %TODO: Need the code for detecting how long simulate too, and adjusting interval that much.

  % Restart the simulate timer
  erlang:send_after(?INTERVAL, self(), simulate),

  {noreply, State};

handle_msg(_Msg, _From, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------


