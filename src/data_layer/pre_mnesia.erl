%% @doc The bridge between pre_data and an mnesia database. Mnesia should
%% already be started because it's an app and not up to this module to do
%% it.

-module(pre_mnesia).
-behavior(pre_data).

-include("log.hrl").

-define(COUNTERS_TABLE, pre_counters).
-define(EXPECTED_TABLES, [schema, ?COUNTERS_TABLE]).

-export([check_setup/0]).
-export([transaction/1, save/1, get_by_id/2, delete/2, search/2]).

%% -----------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------

%% @doc Ensure the mnesia schema and tables exist. Assumes a schema already
%% exists.
-spec check_setup() -> 'ok' | {'error', any()}.
check_setup() ->
	Tables = mnesia:system_info(tables),
	Expected = lists:sort(?EXPECTED_TABLES),
	Got = lists:sort(Tables),
	Needed = Expected -- Got,
	create_tables(Needed).

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

create_tables([]) ->
	ok;

create_tables([?COUNTERS_TABLE | Tail]) ->
	{atomic, ok} = mnesia:create_table(?COUNTERS_TABLE, []),
	create_tables(Tail).

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

