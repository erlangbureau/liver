-module(oliver_maps).

%%
%% This is a wrapper-module for maps and proplists modules
%%

%% API
-export([new/1]).
-export([type/1]).
-export([get/2, get/3]).
-export([reverse/1]).
-export([keys/1]).
-export([is_keyvalue/1]).
-export([is_key/2]).
-export([is_empty/1]).
-export([fold/3]).
-export([put/3]).


%% API
new(map) ->
    #{};
new(list) ->
    [{}].

type(Value) when is_map(Value) ->
    map;
type(Value) when is_list(Value) ->
    list.

is_keyvalue(Data) when is_map(Data); is_list(Data) ->
    true;
is_keyvalue(_Data) ->
    false.

get(Key, List)->
    get(Key, List, undefined).

get(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get(Key, List, Default) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        _ ->
            case lists:member(Key, List) of
                true -> true;
                false -> Default
            end
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
        [{}]  -> true;
        _   -> false
    end.

put(Key, Value, Map) when is_map(Map) ->
    maps:put(Key, Value, Map);
put(Key, Value, [{}]) ->
    [{Key, Value}];
put(Key, Value, List) when is_list(List) ->
    [{Key, Value}|List].

fold(Fun, Init, Map) when is_map(Map) ->
    maps:fold(Fun, Init, Map);
fold(Fun, Init, List) when is_list(List) ->
    foldl(Fun, Init, List).

%% internal
foldl(F, Acc, [{K, V}|Tail]) ->
    foldl(F, F(K, V, Acc), Tail);
foldl(F, Acc, [K|Tail]) ->
    foldl(F, F(K, [], Acc), Tail);
foldl(_F, Acc, []) ->
    Acc.
