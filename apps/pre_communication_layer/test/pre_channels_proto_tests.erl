-module(pre_channels_proto_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(state, {
	ref :: any(),
	socket :: any(),
	transport :: atom(),
	client :: pid(),
	aes_key :: binary(),
	aes_vector :: binary(),
	netstring_cont = 10 :: integer() | term()
}).

-define(AES_BLOCK_SIZE, 16).

%% --------------------------------------------------------------------------------------------------------------------

simple_start_test() ->
	Got = pre_channels_proto:start_link(fake_ref, fake_socket, fake_transport, []),
	?assertMatch({ok, _}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),

	% Teardown
	gen_server:cast(Pid, stop),
	Mon = erlang:monitor(process, Pid),
	receive
		{'DOWN', Mon, process, Pid, _} ->
			ok
	end.

proto_test_() ->
	% Setup
	{setup, fun() ->
		meck:new(fake_transport, [non_strict]),
		meck:new(pre_client, [non_strict])
	end,

	% Test runner
	fun(_) -> [
		{ "can send via ssl",
			fun() ->
				meck:expect(fake_transport, send, fun(_Socket, Message) ->
					Expected = <<"14:{\"foo\":\"Bar!\"},">>,
					?assertEqual(Expected, Message)
				end),
				State = #state{ socket = fake_socket, transport = fake_transport},
				pre_channels_proto:handle_cast({send, [{foo, <<"Bar!">>}]}, State)
			end
		},
		{ "can send via encrypted tcp",
			fun() ->
				Key = <<"770A8A65DA156D24EE2A093277530142">>,
				IV = crypto:rand_bytes(16),

				meck:expect(fake_transport, send, fun(_Socket, Message) ->
					Expected = netstring:encode(aes_encrypt(<<"{\"foo\":\"Bar!\"}">>, Key, IV)),
					?assertEqual(Expected, Message)
				end),
				State = #state{
					socket = fake_socket,
					transport = fake_transport,
					aes_key = Key,
					aes_vector = IV
				},
				pre_channels_proto:handle_cast({send, [{foo, <<"Bar!">>}]}, State)
			end
		},
		{ "can decode ssl message",
			fun() ->
				meck:expect(pre_client, handle_messages, fun(_Pid, _Trans, Messages) ->
					Expected = [{envelope,event,undefined,<<"ping">>,<<"foobar">>}],
					?assertEqual(Expected, Messages)
				end),
				State = #state{ client = fake_client },
				Data = netstring:encode(<<"{\"channel\":\"ping\",\"type\":\"event\",\"contents\":\"foobar\"}">>),
				pre_channels_proto:handle_info({ssl, fake_socket, Data}, State)
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
				pre_channels_proto:handle_info({tcp, fake_socket, Data}, State)
			end
		},
		{ "looks up cookie from initial tcp message",
			fun() ->
				?assert(true)
			end
		},
		{ "rejects the client if the first tcp message does not have a cookie",
			fun() ->
				?assert(true)
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

-endif.
