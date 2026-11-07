-module(liver_clock).

%% The current time for the relative date bounds of the LIVR rules
%% (today, yesterday, tomorrow). A module of its own so that tests can fix
%% the time with meck, as there is no portable way to replace the system
%% clock in Erlang.

%% API
-export([system_time/0]).

%% API
-spec system_time() -> integer().
system_time() ->
    os:system_time(millisecond).
