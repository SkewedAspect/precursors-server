-module(preetu).

-compile(export_all).

wait_for_exit(undefined) ->
	ok;

wait_for_exit(Atom) when is_atom(Atom) ->
	wait_for_exit(whereis(Atom));

wait_for_exit(Pid) when is_pid(Pid) ->
	Mon = erlang:monitor(process, Pid),
	receive
		{'DOWN', Mon, process, Pid, _} ->
			ok
	end.

