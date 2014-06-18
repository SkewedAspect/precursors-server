-module(pre_physical_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

force_test_() ->
	{foreach, fun() ->
		(((pre_physical:new())
			:force_absolute({1, 2, 3}))
			:force_relative({3, 2, 1}))
	end,
	[
		fun(Physical) -> {"applies absolute force correctly", fun() ->
			Got = Physical:apply_force_absolute({-2, -2, -2}),
			?assertEqual({-1, 0, 1}, Got:force_absolute())
		end} end,

		fun(Physical) -> {"applies relative force correctly", fun() ->
			Got = Physical:apply_force_relative({-2, -2, -2}),
			?assertEqual({1, 0, -1}, Got:force_relative())
		end} end
	]}.


torque_test_() ->
	{foreach, fun() ->
		(((pre_physical:new())
			:torque_absolute(quaternion:from_axis_angle(degrees, {0, 1, 0}, 180)))
			:torque_relative(quaternion:from_axis_angle(degrees, {1, 0, 0}, 180)))
	end,
	[
		fun(Physical) -> {"applies absolute torque correctly", fun() ->
			Physical2 = Physical:apply_torque_absolute(quaternion:from_axis_angle(degrees, {0, 0, 1}, 180)),
			Physical3 = Physical2:torque_absolute(quaternion:unit(Physical2:torque_absolute())),
			Physical4 = Physical3:apply_torque_absolute(quaternion:from_axis_angle(degrees, {1, 0, 0}, 180)),
			Got = quaternion:unit(Physical4:torque_absolute()),
			?assertEqual(quaternion:identity(), Got)
		end} end,

		fun(Physical) -> {"applies relative torque correctly", fun() ->
			Physical2 = Physical:apply_torque_relative(quaternion:from_axis_angle(degrees, {0, 0, 1}, 180)),
			Physical3 = Physical2:torque_relative(quaternion:unit(Physical2:torque_relative())),
			Physical4 = Physical3:apply_torque_relative(quaternion:from_axis_angle(degrees, {0, 1, 0}, 180)),
			Got = quaternion:unit(Physical4:torque_relative()),
			?assertEqual(quaternion:identity(), Got)
		end} end
	]}.


simulate_test_() ->
	{foreach, fun() ->
		((((((pre_physical:new())
			:position({1, 1, 1}))
			:linear_velocity({1, -1, 2}))
			:linear_momentum({1, -1, 2}))
			:orientation(
				quaternion:from_axis_angle(
					vector:unit({3, -1, -5}),
					1.3 * math:pi()
				)
			))
			:angular_momentum({5, 0, 3}))
	end,
	[
		fun(Physical) ->
			%io:format("In: ~w\n", [Physical]),
			Got = pre_physical:simulate(10000, Physical),
			io:format("Out: ~w\n", [Got]),

			Expected = [
				{position, {1.01, 0.99, 1.02}},
				{linear_momentum, {1,-1, 2}},
				{orientation, {-0.45379758109327095, 0.4425417360733242, -0.12494483371006153, -0.7632911343362173}},
				{angular_momentum, {5.0, 0.0, 3.0}},
				{force_absolute, {0, 0, 0}},
				{force_relative, {0, 0, 0}},
				{torque_absolute, {0, 0, 0}},
				{torque_relative, {0, 0, 0}},
				{linear_velocity, {1.0,-1.0, 2.0}},
				{angular_velocity, {5.0, 0.0, 3.0}},
				{spin, {0.07712102010052835, -0.9842732176162007, 2.581563520400872, -0.9277868135932753}},
				{mass, 1.0},
				{inverse_mass, 1.0},
				{inertia_tensor, 1.0},
				{inverse_inertia_tensor, 1.0}
			],%),

			{"simulates a test object correctly", lists:map(
				fun({Field, ExpectedVal}) ->
					{atom_to_list(Field), ?_assertEqual(ExpectedVal, Got:Field())}
				end,
				Expected
			)}

			%?assertEqual(Expected,
			%%io:format("pre_physical:to_json(Got) => ~p~n", [pre_physical:to_json(Got)]),
			%%io:format("jsx:encode(pre_physical:to_json(Got)) => ~p~n", [jsx:encode(pre_physical:to_json(Got))]),
			%?assertEqual(Expected,
			%	Got:to_json()
			%	%Got:to_json(fun(Json) ->
			%	%
			%	%end)
			%)
		end
	]}.

-endif.
