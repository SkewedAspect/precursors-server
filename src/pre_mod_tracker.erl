%% @doc Abstract property modification tracker. A modification is an
%% integer value applied to a specific property of a modifiable. A
%% modifiable is a collections of properties with integers as the values.
%%
%% A buff is a modification with a positive value, while a debuff is a 
%% modification with a zero or lower value.
%%
%% In terms of gameplay, a ship is modifiable, with things like the max
%% speed, acceleration, and energy regen being the properties that can
%% be modified.
%%
%% The primary purpose of this module is to keep track of individual
%% modifications so they can be queried for things like name and value,
%% as well as the aggregate value of modifications to a modifiable's
%% properties. As such, this does not acutally keep track of modifiables
%% directly, mearly modifications.

-module(pre_mod_tracker).

-record(pre_mod_tracker, {
	props = dict:new() :: dict()
}).

-export([
	new/0,
	new/1,
	append/4,
	remove/4,
	to_proplist/1
]).

%% =====
%% API
%% =====

new() ->
	new([]).

new(PropNames) ->
	PropDict = lists:foldl(fun(PropName, Acc) ->
		dict:store(PropName, [], Acc)
	end, dict:new(), PropNames),
	#pre_mod_tracker{props = PropDict}.

append(PropName, ModName, ModValue, Mod) ->
	Dict = Mod#?MODULE.props,
	Dict2 = dict:append(PropName, {ModName, ModValue}, Dict),
	Mod#?MODULE{props = Dict2}.

remove(PropName, ModName, ModValue, Mod) ->
	Dict = Mod#?MODULE.props,
	ModFun = fun(List) ->
		lists:delete({ModName, ModValue}, List)
	end,
	Dict2 = dict:update(PropName, ModFun, [], Dict),
	Mod#?MODULE{props = Dict2}.

to_proplist(#pre_mod_tracker{props = Props}) ->
	DictList = dict:to_list(Props),
	lists:map(fun({Key, ModList}) ->
		{Key, sum_mods(ModList)}
	end, DictList).

%% =====
%% internal
%% =====

sum_mods(Mods) ->
	lists:foldl(fun({_Name, Value}, Acc) ->
		Acc + Value
	end, 0, Mods).

