%% @doc The bridge between pre_data and an mnesia database. Mnesia should
%% already be started because it's an app and not up to this module to do
%% it.

-module(pre_mnesia).
-behavior(pre_data).

-define(COUNTERS_TABLE, pre_counters).
-define(EXPECTED_TABLES, [schema, ?COUNTERS_TABLE, pre_rec_account]).

-export([check_setup/0, check_setup/1]).
-export([transaction/1, save/1, get_by_id/2, delete/2, search/2]).

%% -----------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------

%% @doc Same as {@link check_setup/1} passed 'true'.
%% @see check_setup/1
-spec check_setup() -> 'ok' | {'error', any()}.
check_setup() ->
	check_setup(true).

%% @doc Ensure the mnesia schema and tables exist. This may stop mnesia to
%% create a schema on disk if 'true' is passed. Any other value and mnesia
%% does not perist the data, meaning once mnesia is stopped, it will be
%% lost
-spec check_setup(Persist :: boolean()) -> 'ok' | {'error', any()}.
check_setup(Persist) ->
	maybe_recreate_schema(Persist),
	Tables = mnesia:system_info(tables),
	Expected = lists:sort(?EXPECTED_TABLES),
	Got = lists:sort(Tables),
	Needed = Expected -- Got,
	create_tables(Needed, Persist).

%% -----------------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------------

%% @hidden
save(Record) ->
	Table = element(1, Record),
	MaybeId = element(2, Record),
	SaveRecord = maybe_fix_id(Table, MaybeId, Record),
	case mnesia:write(SaveRecord) of
		ok ->
			{ok, SaveRecord};
		Else ->
			Else
	end.

%% @hidden
get_by_id(Type, Id) ->
	case mnesia:read(Type, Id) of
		[Rec] ->
			{ok, Rec};
		[] ->
			{error, notfound};
		MultiRec ->
			{error, {multiple, MultiRec}}
	end.

%% @hidden
delete(Type, Id) ->
	mnesia:delete({Type, Id}).

%% @hidden
search(Type, Params) ->
	Matchspec = make_matchspec(Type, Params),
	Recs = mnesia:select(Type, Matchspec),
	{ok, Recs}.
		
%% @hidden
transaction(Fun) ->
	case mnesia:transaction(Fun) of
		{atomic, Out} ->
			Out;
		{aborted, Wut} ->
			{error, Wut}
	end.

%% -----------------------------------------------------------------------
%% Internal
%% -----------------------------------------------------------------------

maybe_fix_id(RecName, undefined, Record) ->
	NewId = mnesia:dirty_update_counter(?COUNTERS_TABLE, RecName, 1),
	setelement(2, Record, NewId);

maybe_fix_id(_RecName, _Id, Record) ->
	Record.

maybe_recreate_schema(true) ->
	Node = node(),
	case mnesia:table_info(schema, disc_copies) of
		[] ->
			{atomic, ok} = mnesia:change_table_copy_type(schema, Node, disc_copies);
		[Node] ->
			ok;
		DiscNodes ->
			case lists:member(Node, DiscNodes) of
				true ->
					ok;
				false ->
					{ok, _} = mnesia:change_config(extra_db_nodes, DiscNodes),
					{atomic, ok} = mnesia:change_table_copy_type(schema, disc_copies)
			end
	end;

maybe_recreate_schema(_) ->
	'ok'.

create_tables(TableNames, Persist) ->
	DiscNodes = case Persist of
		true -> [node() | nodes()];
		_ -> []
	end,
	lists:foreach(fun(TableName) ->
		{atomic, ok} = create_table(TableName, DiscNodes)
	end, TableNames).

create_table(?COUNTERS_TABLE, DiscNodes) ->
	TableOpts = [
		{disc_copies, DiscNodes}
	],
	{atomic, ok} = mnesia:create_table(?COUNTERS_TABLE, TableOpts);

create_table(pre_rec_account, DiscNodes) ->
	{atomic, ok} = mnesia:create_table(pre_rec_account, [
		{attributes, pre_rec_account:field_names()},
		{disc_copies, DiscNodes}
	]).

make_matchspec(Table, Params) ->
	Attributes = mnesia:table_info(Table, attributes),
	Numbered = lists:zip(Attributes, lists:seq(1, length(Attributes))),
	Dollared = lists:map(fun({Attr, N}) ->
		NList = integer_to_list(N),
		MatchAtom = list_to_atom("$" ++ NList),
		{Attr, MatchAtom}
	end, Numbered),
	{_Attrs, DollaredAlone} = lists:unzip(Dollared),
	MatchRec = list_to_tuple([Table | DollaredAlone]),
	NormalParams = normalize_params(Params),
	MatchSpecParams = make_match_guard(NormalParams, Dollared),
	[{MatchRec, MatchSpecParams, ['$_']}].

normalize_params(Params) ->
	lists:map(fun normalize_param/1, Params).

normalize_param({Key, Value}) -> {Key, '==', Value};
normalize_param(P) -> P.

make_match_guard(In, Atoms) ->
	FilteredIn = lists:filter(fun({Key, _, _}) ->
		case proplists:get_value(Key, Atoms) of
			undefined -> false;
			_ -> true
		end
	end, In),
	lists:map(fun({Key, Op, Arg}) ->
		Atom = proplists:get_value(Key, Atoms),
		{Op, Atom, Arg}
	end, FilteredIn).

