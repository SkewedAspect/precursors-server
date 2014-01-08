%% @doc The bridge between pre_data and an mnesia database. Mnesia should
%% already be started because it's an app and not up to this module to do
%% it.

-module(pre_mnesia).

-include("log.hrl").

-define(COUNTERS_TABLE, pre_counters).
-define(EXPECTED_TABLES, [schema, ?COUNTERS_TABLE]).

-export([check_setup/0]).
-export([transaction/1, save/1, get_by_id/2, delete/2]).

%% @doc Ensure the mnesia schema and tables exist. Assumes a schema already
%% exists.
-spec check_setup() -> 'ok' | {'error', any()}.
check_setup() ->
	Tables = mnesia:system_info(tables),
	Expected = lists:sort(?EXPECTED_TABLES),
	Got = lists:sort(Tables),
	Needed = Expected -- Got,
	create_tables(Needed).

%% @hidden
save(Record) ->
	?info("pdict: ~p", [get()]),
	Table = element(1, Record),
	MaybeId = element(2, Record),
	SaveRecord = maybe_fix_id(Table, MaybeId, Record),
	case mnesia:write(SaveRecord) of
		ok ->
			{ok, SaveRecord};
		Else ->
			Else
	end.

get_by_id(Type, Id) ->
	case mnesia:read(Type, Id) of
		[Rec] ->
			{ok, Rec};
		[] ->
			{error, notfound};
		MultiRec ->
			{error, {multiple, MultiRec}}
	end.

delete(Type, Id) ->
	mnesia:delete({Type, Id}).

%% @hidden
transaction(Fun) ->
	case mnesia:transaction(Fun) of
		{atomic, Out} ->
			Out;
		{aborted, Wut} ->
			{error, Wut}
	end.

maybe_fix_id(RecName, undefined, Record) ->
	NewId = mnesia:dirty_update_counter(?COUNTERS_TABLE, RecName, 1),
	setelement(2, Record, NewId);

maybe_fix_id(_RecName, _Id, Record) ->
	Record.

create_tables([]) ->
	ok;

create_tables([?COUNTERS_TABLE | Tail]) ->
	{atomic, ok} = mnesia:create_table(?COUNTERS_TABLE, []),
	create_tables(Tail).

