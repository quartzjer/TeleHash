-module(log).

-export([start/0]).
-export([info/1, warn/1, error/1]).

% --- api ---

start() ->
    switch:add_sup_handler(?MODULE, none).

info(Info) ->
    error_logger:info_report([{pid, self()} | Info]).

warn(Warn) ->
    error_logger:warning_report([{pid, self()} | Warn]).

error(Error) ->
    error_logger:error_report([{pid, self()} | Error]).

% --- end ---
