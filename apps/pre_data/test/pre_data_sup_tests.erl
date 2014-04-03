-module(pre_data_sup_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
	mnesia:start(),
	Got = pre_data_sup:start_link(),
	?assertMatch({ok, _Pid}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	pre_data_sup:stop(),
	mnesia:stop(),
	mnesia:delete_schema([node()]).

-endif.
