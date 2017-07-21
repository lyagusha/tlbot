-module(tlbot).
-export([start/0]).

start() ->
    sync:go(),
    application:ensure_all_started(tlbot).
