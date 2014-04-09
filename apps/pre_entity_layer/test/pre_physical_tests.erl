-module(pre_physical_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

behavior_test_() ->
	{foreach, fun() ->
		((((pre_physical:new()
			):force_absolute({1, 2, 3})
			):force_relative({3, 2, 1})
			):torque_absolute(quaternion:from_axis_angle(degrees, {0, 1, 0}, 180))
			):torque_relative(quaternion:from_axis_angle(degrees, {1, 0, 0}, 180))
	end,
	[
		fun(Physical) -> {"applies absolute force correctly", fun() ->
			Got = Physical:apply_force_absolute({-2, -2, -2}),
			?assertEqual({-1, 0, 1}, Got:force_absolute())
		end} end,

		fun(Physical) -> {"applies relative force correctly", fun() ->
			Got = Physical:apply_force_relative({-2, -2, -2}),
			?assertEqual({1, 0, -1}, Got:force_relative())
		end} end,

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

-endif.
