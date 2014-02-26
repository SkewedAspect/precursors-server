-module(pre_channels_proto_tests).

-include_lib("eunit/include/eunit.hrl").

simple_start_test() ->
	Got = pre_channels_proto:start_link(fake_ref, fake_socket, fake_transport, []),
	?assertMatch({ok, _}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	exit(Pid, normal).
