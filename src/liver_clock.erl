-module(liver_clock).

%% The current time for the relative date bounds of the LIVR rules
%% (today, yesterday, tomorrow). Tests can fix it with set_time/1, as there
%% is no portable way to replace the system clock in Erlang.

%% API
-export([system_time/0]).
-export([set_time/1]).

-define(FIXED_TIME_KEY, liver_clock_fixed_time).

%% API

%% Milliseconds since the epoch: the system time, or the time fixed with
%% set_time/1.
-spec system_time() -> integer().
system_time() ->
    case persistent_term:get(?FIXED_TIME_KEY, undefined) of
        undefined -> os:system_time(millisecond);
        Ms -> Ms
    end.

%% Fix the time (milliseconds since the epoch, UTC), e.g. in tests;
%% undefined goes back to the system time. It is a persistent term, so
%% changing it is costly: not for production use.
-spec set_time(integer() | undefined) -> ok.
set_time(undefined) ->
    _ = persistent_term:erase(?FIXED_TIME_KEY),
    ok;
set_time(Ms) when is_integer(Ms) ->
    persistent_term:put(?FIXED_TIME_KEY, Ms).
