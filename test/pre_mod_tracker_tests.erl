-module(pre_mod_tracker_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test_() -> [

		{"create a new mod tracker", fun() ->
			Got = pre_mod_tracker:new()
		end},

		{"to proplsit with empty tracker", fun() ->
			Mod = pre_mod_tracker:new(),
			?assertEqual([], pre_mod_tracker:to_proplist(Mod))
		end},

		{"create tracker with list of properties", fun() ->
			Got = pre_mod_tracker:new([a, b, c, d])
		end},

		{"to proplist with empty modifiers", fun() ->
			PropNames = [a,b,c,d],
			Mod = pre_mod_tracker:new(PropNames),
			Expected = lists:map(fun(Name) ->
				{Name, 0}
			end, PropNames),
			?assertEqual(Expected, pre_mod_tracker:to_proplist(Mod))
		end}
		
	].

usage_test_() ->
	usage_test(undefined, [
		"create empty tracker",
		"append modifer \"m1\" with value 3 to undef property 'a'",
		"append modifier \"m2\" with value -7 to undef property 'b'",
		"append modifier \"m3\" with value -24 to property 'a'",
		"remove modifier \"m1\" with value 3 to property 'a'",
		"append modifier \"m2\" with value 3 to property 'b'"
	]).

usage_test(_Mod, []) ->
	{generator, fun() -> [] end};

usage_test(Mod, [Name | Tail]) ->
	{NewMod, Test} = usage_test_gen(Name, Mod),
	{generator, fun() ->
		[Test, usage_test(NewMod, Tail)]
	end}.

usage_test_gen("create empty tracker" = Name, Mod) ->
	NewMod = pre_mod_tracker:new(),
	Test = fun() ->
		?assertEqual([], pre_mod_tracker:to_proplist(NewMod))
	end,
	{NewMod, {Name, Test}};

usage_test_gen("append modifer \"m1\" with value 3 to undef property 'a'" = Name, Mod) ->
	NewMod = pre_mod_tracker:append(a, <<"m1">>, 3, Mod),
	Test = fun() ->
		?assertEqual([{a, 3}], pre_mod_tracker:to_proplist(NewMod))
	end,
	{NewMod, {Name, Test}};

usage_test_gen("append modifier \"m2\" with value -7 to undef property 'b'" = Name, Mod) ->
	NewMod = pre_mod_tracker:append(b, <<"m2">>, -7, Mod),
	Test = fun() ->
		?assertEqual([{a, 3}, {b, -7}], NewMod:to_proplist())
	end,
	{NewMod, {Name, Test}};

usage_test_gen("append modifier \"m3\" with value -24 to property 'a'" = Name, Mod) ->
	NewMod = Mod:append(a, <<"m3">>, -24),
	Test = fun() ->
		?assertEqual([{a, -21}, {b, -7}], pre_mod_tracker:to_proplist(NewMod))
	end,
	{NewMod, {Name, Test}};

usage_test_gen("remove modifier \"m1\" with value 3 to property 'a'" = Name, Mod) ->
	NewMod = pre_mod_tracker:remove(a, <<"m1">>, 3, Mod),
	Test = fun() ->
		?assertEqual([{a, -24}, {b, -7}], pre_mod_tracker:to_proplist(NewMod))
	end,
	{NewMod, {Name, Test}};

usage_test_gen("append modifier \"m2\" with value 3 to property 'b'" = Name, Mod) ->
	NewMod = pre_mod_tracker:append(b, <<"m2">>, 3, Mod),
	Test = fun() ->
		?assertEqual([{a, -24}, {b, -4}], pre_mod_tracker:to_proplist(NewMod))
	end,
	{NewMod, {Name, Test}}.

-endif.
