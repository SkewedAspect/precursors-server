-module(pre_client_tests).

-include_lib("eunit/include/eunit.hrl").

%% simple_start_test() ->
%% 	pre_client:ensure_ets(),
%% 	Got = pre_client:start_link(fake_proto, fake_cookie),
%% 	?assertMatch({ok, _}, Got),
%% 	{ok, Pid} = Got,
%% 	?assert(is_pid(Pid)),
%%
%% 	% Teardown
%% 	gen_server:cast(Pid, stop),
%% 	Mon = erlang:monitor(process, Pid),
%% 	receive
%% 		{'DOWN', Mon, process, Pid, _} ->
%% 			ok
%% 	end.
%%
%% proto_test_() ->
%% 	% Setup
%% 	{setup, fun() ->
%% 		meck:new(fake_transport, [non_strict]),
%% 		pre_client:ensure_ets(),
%% 		{ok, Pid} = pre_client:start_link(fake_proto, fake_cookie),
%% 		Pid
%% 	end,
%%
%% 		% Teardown
%% 		fun(Pid) ->
%% 			gen_server:cast(Pid, stop),
%% 			Mon = erlang:monitor(process, Pid),
%% 			receive
%% 				{'DOWN', Mon, process, Pid, _} ->
%% 					ok
%% 			end
%% 		end,
%%
%% 		% Test runner
%% 		fun(_Pid) -> [
%% 			{ "can stuff",
%% 				fun() ->
%% 					?assert(true)
%% 				end
%% 			},
%% 			{ "can pass",
%% 				fun() ->
%% 					?assert(true)
%% 				end
%% 			}]
%% 		end
%% 	}.
