-module(liver_maps).

%%
%% This is a wrapper-module for maps and proplists modules
%%

%% API
-export([new/1]).
-export([type/1]).
-export([get/2, get/3]).
-export([reverse/1]).
-export([keys/1]).
-export([is_empty/1]).
-export([put/3]).
-export([map/2]).


%% API
new(map) ->
    #{};
new(proplist) ->
    [{}].

type(Value) when is_map(Value) ->
    map;
type(Value) when is_list(Value) ->
    proplist.

get(Key, List)->
    get(Key, List, undefined).

get(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get(Key, List, Default) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        _ -> Default
    end;
get(_Key, _Map, Default) ->
    Default.

reverse(Map) when is_map(Map) ->
    Map;
reverse(List) when is_list(List) ->
    lists:reverse(List).

keys(Map) when is_map(Map) ->
    maps:keys(Map);
keys(List) when is_list(List) ->
    [K || {K, _V} <- List].

is_empty(Map) when is_map(Map) ->
    maps:size(Map) =:= 0;
is_empty(List) when is_list(List) ->
    case List of
        [{}]  -> true;
        _   -> false
    end.

put(Key, Value, Map) when is_map(Map) ->
    maps:put(Key, Value, Map);
put(Key, Value, [{}]) ->
    [{Key, Value}];
put(Key, Value, List) when is_list(List) ->
    [{Key, Value}|List].

map(Fun, Map) when is_map(Map) ->
    maps:map(Fun, Map);
map(Fun, List) when is_list(List) ->
    [{K, Fun(K, V)}|| {K, V} <- List].
