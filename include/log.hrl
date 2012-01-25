%% because the loggers rarely include everything I'd like to see,
%% this wraps 'em up.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(debug(Message), ?debugFmt("[~p][~p][~p]~n	DEBUG: ~s~n", [erlang:localtime(), node(), self(), Message])).
-define(debug(Message, Args), ?debugFmt("[~p][~p][~p]~n	DEBUG: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).

-define(info(Message), ?debugFmt("[~p][~p][~p]~n	INFO: ~s~n", [erlang:localtime(), node(), self(), Message])).
-define(info(Message, Args), ?debugFmt("[~p][~p][~p]~n	INFO: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).

-define(notice(Message), ?debugFmt("[~p][~p][~p]~n	NOTICE: ~s~n", [erlang:localtime(), node(), self(), Message])).
-define(notice(Message, Args), ?debugFmt("[~p][~p][~p]~n	NOTICE: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).

-define(warning(Message), ?debugFmt("[~p][~p][~p]~n	WARNING: ~s~n", [erlang:localtime(), node(), self(), Message])).
-define(warning(Message, Args), ?debugFmt("[~p][~p][~p]~n	WARNING: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).

-define(error(Message), ?debugFmt("[~p][~p][~p]~n	ERROR: ~s~n", [erlang:localtime(), node(), self(), Message])).
-define(error(Message, Args), ?debugFmt("[~p][~p][~p]~n	ERROR: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).

-define(critical(Message), ?debugFmt("[~p][~p][~p]~n	CRITICAL: ~s~n", [erlang:localtime(), node(), self(), Message])).
-define(critical(Message, Args), ?debugFmt("[~p][~p][~p]~n	CRITICAL: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).

-define(alert(Message), ?debugFmt("[~p][~p][~p]~n	ALERT: ~s~n", [erlang:localtime(), node(), self(), Message])).
-define(alert(Message, Args), ?debugFmt("[~p][~p][~p]~n	ALERT: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).

-define(emergency(Message), ?debugFmt("[~p][~p][~p]~n	EMERGENCY: ~s~n", [erlang:localtime(), node(), self(), Message])).
-define(emergency(Message, Args), ?debugFmt("[~p][~p][~p]~n	EMERGENCY: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).

-else.

-compile([{parse_transform, lager_transform}]).

-define(debug(Message), lager:debug(Message)).
-define(debug(Message, Args), lager:debug(Message, Args)).

-define(info(Message), lager:info(Message)).
-define(info(Message, Args), lager:info(Message, Args)).

-define(notice(Message), lager:notice(Message)).
-define(notice(Message, Args), lager:notice(Message, Args)).

-define(warning(Message), lager:warning(Message)).
-define(warning(Message, Args), lager:warning(Message, Args)).

-define(error(Message), lager:error(Message)).
-define(error(Message, Args), lager:error(Message, Args)).

-define(critical(Message), lager:critical(Message).
-define(critical(Message, Args), lager:critical(Message, Args)).

-define(alert(Message), lager:alert(Message)).
-define(alert(Message, Args), later:alert(Message, Args)).

-define(emergency(Message), lager:emergency(Message)).
-define(emergency(Message, Args), lager:emergency(Message, Args)).

-endif.
