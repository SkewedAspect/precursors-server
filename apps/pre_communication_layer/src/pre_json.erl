%%% @doc JSON module - wraps jsx to provide some convenience behavior.
%%%
%%% @copyright 2013 David H. Bronke
%%% Licensed under the MIT license; see the LICENSE file for details.

-module(pre_json).

% external api
-export([to_json/1, to_term/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% External API
%% --------------------------------------------------------------------------------------------------------------------

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Convert the given Terms to binary as JSON using jsx, removing any object keys whose values are 'undefined'.

to_json(Terms) ->
	jsx:to_json(Terms, [{pre_encode, fun(Term) -> sanitize_objects(Term) end}]).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Parse the given binary as JSON using jsx, replacing keys with existing atom equivalents if available.

to_term(Bin) ->
	jsx:to_term(Bin, [{labels, binary}, {post_decode, fun(Data) -> obj_keys_to_atom(Data) end}]).

%% --------------------------------------------------------------------------------------------------------------------
%% Internal API
%% --------------------------------------------------------------------------------------------------------------------

sanitize_objects([{_, _} | _] = Object) ->
	sanitize_object_properties(Object);

sanitize_objects(Term) ->
	Term.


sanitize_object_properties([]) ->
	[];

sanitize_object_properties([{_Key, undefined} | Rest]) ->
	sanitize_object_properties(Rest);

sanitize_object_properties([{Key, Value} | Rest]) when is_atom(Value), Value =/= true, Value =/= false, Value =/= null ->
	[{Key, atom_to_binary(Value, utf8)} | sanitize_object_properties(Rest)];

sanitize_object_properties([Pair | Rest]) ->
	[Pair | sanitize_object_properties(Rest)].

%% --------------------------------------------------------------------------------------------------------------------

obj_keys_to_atom([{_, _} | _] = Object) ->
	keys_to_existing_atom_or_bin(Object);

obj_keys_to_atom(Data) ->
	Data.


keys_to_existing_atom_or_bin([]) ->
	[];

keys_to_existing_atom_or_bin([{Key, Value} | Rest]) ->
	[{existing_atom_or_bin(Key), Value} | keys_to_existing_atom_or_bin(Rest)].


existing_atom_or_bin(Bin) ->
	try binary_to_existing_atom(Bin, utf8)
	catch
		error:badarg ->
			Bin
	end.
