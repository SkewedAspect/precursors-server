-module(pre_hit_calc).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([roll/4]).

roll(Crit, Hit, Block, Dodge) ->
	Total = Crit + Hit + Block + Dodge,
	Names = [crit, hit, block, dodge],
	Zipped = lists:zip(Names, [Crit, Hit, Block, Dodge]),
	N = random:uniform(Total),
	resolve(N, Zipped).

resolve(N, [{_Type, Val} | Tail]) when N > Val ->
	NewN = N - Val,
	resolve(NewN, Tail);

resolve(N, [{Type, Val} | _]) ->
	{Type, N / Val}.

-ifdef(TEST).

roll_test() ->
	{_Type, Result} = roll(1, 1, 1, 1),
	Result == 1.

-endif.

