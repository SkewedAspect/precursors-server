-module(pre_sim_worker_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
	pre_sim_worker:init_node(),
	Got = pre_sim_worker:start_link(),
	?assertMatch({ok, _Pid}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	preetu:kill(Pid).

functionality_test_() ->
	{setup, fun() ->
		{ok, Worker} = pre_sim_worker:start_link(),
		meck:new(callback, [non_strict]),
		Worker
	end,
	fun(Worker) ->
		meck:unload(callback),
		preetu:kill(Worker)
	end,
	fun(Worker) -> [

		{"can add an entity", fun() ->
			EntityID = 1,
			Got = pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),
			?assertEqual(ok, Got)
		end},

		{"can remove an entity", fun() ->
			EntityID = 1,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			?assertEqual(ok, pre_sim_worker:remove_entity(EntityID)),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, remove}], EntityUpdates)
		end},

		{"can get an entity's state", fun() ->
			EntityID = 1,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			Got = pre_sim_worker:get_entity_state(EntityID),

			?assertEqual({ok, {some, entity, state}}, Got)
		end},

		{"can update an entity's position", fun() ->
			EntityID = 1,
			Value = foo,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			pre_sim_worker:update_position(EntityID, Value),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, {update_position, Value}}], EntityUpdates)
		end},

		{"can update an entity's linear momentum", fun() ->
			EntityID = 1,
			Value = foo,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			pre_sim_worker:update_linear_momentum(EntityID, Value),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, {update_linear_momentum, Value}}], EntityUpdates)
		end},

		{"can update an entity's orientation", fun() ->
			EntityID = 1,
			Value = foo,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			pre_sim_worker:update_orientation(EntityID, Value),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, {update_orientation, Value}}], EntityUpdates)
		end},

		{"can update an entity's angular momentum", fun() ->
			EntityID = 1,
			Value = foo,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			pre_sim_worker:update_angular_momentum(EntityID, Value),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, {update_angular_momentum, Value}}], EntityUpdates)
		end},

		{"can apply an absolute force to an entity", fun() ->
			EntityID = 1,
			Value = foo,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			pre_sim_worker:apply_force_absolute(EntityID, Value),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, {apply_force_absolute, Value}}], EntityUpdates)
		end},

		{"can apply an relative force to an entity", fun() ->
			EntityID = 1,
			Value = foo,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			pre_sim_worker:apply_force_relative(EntityID, Value),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, {apply_force_relative, Value}}], EntityUpdates)
		end},

		{"can apply an absolute torque to an entity", fun() ->
			EntityID = 1,
			Value = foo,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			pre_sim_worker:apply_torque_absolute(EntityID, Value),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, {apply_torque_absolute, Value}}], EntityUpdates)
		end},

		{"can apply an relative torque to an entity", fun() ->
			EntityID = 1,
			Value = foo,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			pre_sim_worker:apply_torque_relative(EntityID, Value),

			EntityUpdates = get_latest_updates(EntityID),
			?assertEqual([{1, {apply_torque_relative, Value}}], EntityUpdates)
		end},

		{"can simulate an entity's physical movement", fun() ->
			EntityID = 1,
			pre_sim_worker:add_entity(Worker, EntityID, callback, {some, entity, state}),

			meck:expect(callback, simulate, fun(_Updates, _State) -> {[], {new, state}} end),

			pre_sim_worker:simulate(),

			Got = pre_sim_worker:get_entity_state(EntityID),
			?assertEqual({ok, {new, state}}, Got)
		end}

	] end}.

get_latest_updates(EntityID) ->
	[{EntityID, _WorkerPid, WorkerUpdatesTable}] = ets:lookup(pre_sim_entity_table, EntityID),
	EntityUpdates = ets:lookup(WorkerUpdatesTable, EntityID),
	ets:delete(WorkerUpdatesTable, EntityID),
	EntityUpdates.

-endif.


%	add_entity/4,
%	remove_entity/1,
%	get_entity_state/1,
%	update_position/2,
%	update_linear_momentum/2,
%	update_orientation/2,
%	update_angular_momentum/2,
%	apply_force_absolute/2,
%	apply_force_relative/2,
%	apply_torque_absolute/2,
%	apply_torque_relative/2
%
%	init_node/0,
%	start_link/0,
%	simulate/0
