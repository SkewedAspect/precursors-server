-module(pre_server_app_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
		{ok, Started} = application:ensure_all_started(precursors_server),
		lists:foreach(fun(App) ->
			application:stop(App)
		end, lists:reverse(Started)).

-endif.

