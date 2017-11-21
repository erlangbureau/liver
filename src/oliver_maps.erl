-module(oliver_maps).

%% API
-export([new/1]).
-export([type/1]).
-export([get/2, get/3]).
-export([keys/1]).
-export([is_key/2]).
-export([is_empty/1]).
-export([fold/3]).
-export([put/3]).


%% API
new(map) ->
    #{};
new(list) ->
    [].

type(Value) when is_map(Value) ->
    map;
type(Value) when is_list(Value) ->
    list.

get(Key, List)->
    get(Key, List, undefined).

get(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get(Key, List, Default) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        _ -> Default
    end.

keys(Map) when is_map(Map) ->
    maps:keys(Map);
keys(List) when is_list(List) ->
    [K || {K, _V} <- List].

is_key(Key, Map) when is_map(Map) ->
    maps:is_key(Key, Map);
is_key(Key, List) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        false -> lists:member(Key, List);
        _True -> true
    end.

is_empty(Map) when is_map(Map) ->
    maps:size(Map) =:= 0;
is_empty(List) when is_list(List) ->
    case List of
        []  -> true;
        _   -> false
    end.

fold(Fun, Init, Map) when is_map(Map) ->
    maps:fold(Fun, Init, Map);
fold(Fun, Init, List) when is_list(List) ->
    foldl(Fun, Init, List).

put(Key, Value, Map) when is_map(Map) ->
    maps:put(Key, Value, Map);
put(Key, Value, List) when is_list(List) ->
    [{Key, Value}|List].

%% internal
foldl(F, Acc, [{K, V}|Tail]) ->
    foldl(F, F(K, V, Acc), Tail);
foldl(_F, Acc, []) ->
    Acc.
