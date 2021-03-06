-module(pre_channels_proto_tests).

-include("pre_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
	ref :: any(),
	socket :: any(),
	transport :: atom(),
	client :: pid(),
	clientmon :: reference(),
	aes_key :: binary(),
	aes_vector :: binary(),
	netstring_cont = 10 :: integer() | term()
}).

-define(AES_BLOCK_SIZE, 16).

%% --------------------------------------------------------------------------------------------------------------------

simple_start_test_d() ->
	Got = pre_channels_proto:start_link(fake_ref, fake_socket, fake_transport, []),
	?assertMatch({ok, _}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),

	% Teardown
	Mon = erlang:monitor(process, Pid),
	gen_server:cast(Pid, stop),

	% Wait for the pid to exit
	receive
		{'DOWN', Mon, process, Pid, _} ->
			ok
	end.

proto_test_() ->
	% Setup
	{setup, fun() ->
		meck:new(fake_transport, [non_strict]),
		meck:expect(fake_transport, setopts, fun(_, _) -> ok end),
		meck:new(pre_client)
	end,

	% Teardown
	fun(_) ->
		meck:unload()
	end,

	% Test runner
	fun(_) -> [
		{ "can send via ssl",
			fun() ->
				meck:expect(fake_transport, send, fun(_Socket, Message) ->
					Expected = <<"46:{\"type\":\"undefined\",\"contents\":{\"foo\":\"Bar!\"}},">>,
					?assertEqual(Expected, Message)
				end),
				State = #state{ socket = fake_socket, transport = fake_transport},
				pre_channels_proto:handle_cast({send, #envelope{contents = [{foo, <<"Bar!">>}]}}, State),
				?assert(meck:validate(fake_transport))
			end
		},
		{ "can send via encrypted tcp",
			fun() ->
				Key = <<"770A8A65DA156D24EE2A093277530142">>,
				IV = crypto:rand_bytes(16),

				meck:expect(fake_transport, send, fun(_Socket, Message) ->
					Expected = netstring:encode(aes_encrypt(<<"{\"type\":\"undefined\",\"contents\":{\"foo\":\"Bar!\"}}">>, Key, IV)),
					?assertEqual(Expected, Message)
				end),
				State = #state{
					socket = fake_socket,
					transport = fake_transport,
					aes_key = Key,
					aes_vector = IV
				},
				pre_channels_proto:handle_cast({send, #envelope{contents = [{foo, <<"Bar!">>}]}}, State),
				?assert(meck:validate(fake_transport))
			end
		},
		{ "can decode ssl message",
			fun() ->
				meck:expect(pre_client, handle_messages, fun(_Pid, _Trans, Messages) ->
					Expected = [{envelope,event,undefined,<<"ping">>,<<"foobar">>}],
					?assertEqual(Expected, Messages)
				end),
				State = #state{ client = fake_client, transport = fake_transport },
				Data = netstring:encode(<<"{\"channel\":\"ping\",\"type\":\"event\",\"contents\":\"foobar\"}">>),
				pre_channels_proto:handle_info({ssl, fake_socket, Data}, State),
				?assert(meck:validate(pre_client))
			end
		},
		{ "can decode encrypted tcp message",
			fun() ->
				Key = <<"770A8A65DA156D24EE2A093277530142">>,
				IV = crypto:rand_bytes(16),

				meck:expect(pre_client, handle_messages, fun(_Pid, _Trans, Messages) ->
					Expected = [{envelope,event,undefined,<<"ping">>,<<"foobar">>}],
					?assertEqual(Expected, Messages)
				end),
				State = #state{
					client = fake_client,
					socket = fake_socket,
					transport = fake_transport,
					aes_key = Key,
					aes_vector = IV
				},
				Data = netstring:encode(aes_encrypt(<<"{\"channel\":\"ping\",\"type\":\"event\",\"contents\":\"foobar\"}">>, Key, IV)),
				pre_channels_proto:handle_info({tcp, fake_socket, Data}, State),
				?assert(meck:validate(pre_client))
			end
		},
		{ "looks up cookie from initial tcp message",
			fun() ->
				meck:expect(pre_client, connect_tcp, fun(_Pid, Cookie, _RequestID) ->
					Expected = <<"foobar">>,
					?assertEqual(Expected, Cookie)
				end),
				meck:expect(pre_client, get_aes, fun(_Pid) ->
					Key = <<"770A8A65DA156D24EE2A093277530142">>,
					IV = crypto:rand_bytes(16),
					{Key, IV}
				end),
				State = #state{ transport = fake_transport },
				Data = netstring:encode(<<"{\"channel\":\"control\",\"type\":\"request\",\"contents\":{\"cookie\":\"foobar\"}}">>),
				pre_channels_proto:handle_info({tcp, fake_socket, Data}, State),
				?assert(meck:validate(pre_client))
			end
		}]
	end
	}.

%% --------------------------------------------------------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------------------------------------------------------

get_pkcs5_padding(Packet) ->
	PacketLength = byte_size(Packet),
	PaddingNeeded = ?AES_BLOCK_SIZE - (PacketLength rem ?AES_BLOCK_SIZE),
	binary:copy(<<PaddingNeeded/integer>>, PaddingNeeded).

aes_encrypt(Packet, AESKey, AESVector) ->
	Padding = get_pkcs5_padding(Packet),
	Padded = <<Packet/binary, Padding/binary>>,
	crypto:aes_cbc_128_encrypt(AESKey, AESVector, Padded).
