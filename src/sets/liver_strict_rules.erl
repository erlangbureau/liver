-module(liver_strict_rules).

%% API
%% common rules
-export([required/3]).
-export([default/3]).
-export([is_null/3]).
-export([is_not_null/3]).
-export([is_undefined/3]).
-export([is_not_undefined/3]).

%% string rules
-export([is_string/3, to_string/3]).
-export([is_bstring/3, to_bstring/3]).
-export([is_atom/3, to_atom/3]).

%% numeric rules
-export([is_integer/3, to_integer/3]).

%% boolean rules
-export([is_boolean/3, to_boolean/3]).

%% list rules
-export([is_list/3]).

%% proplist rules
-export([is_proplist/3]).

%% map rules
-export([is_map/3]).

%% nested elements
-export([nested_map/3]).
-export([nested_list/3]).
-export([nested_proplist/3]).


-include("liver_rules.hrl").


%% API
required(_Args, ?MISSED_FIELD_VALUE, _Opts) ->
    {error, required};
required(_Args, Value, _Opts) ->
    {ok, Value}.

default([Default], _Value, Opts) ->
    default(Default, _Value, Opts);
default(Default, ?MISSED_FIELD_VALUE, _Opts) ->
    {ok, Default};
default(_Default, Value, _Opts) ->
    {ok, Value}.



is_null(_Args, Value = null, _Opts) ->
    {ok, Value};
is_null(_Args, _Value, _Opts) ->
    {error, not_null}.

is_not_null(_Args, null, _Opts) ->
    {error, cannot_be_null};
is_not_null(_Args, Value, _Opts) ->
    {ok, Value}.

is_undefined(_Args, Value = undefined, _Opts) ->
    {ok, Value};
is_undefined(_Args, _Value, _Opts) ->
    {error, not_undefined}.

is_not_undefined(_Args, undefined, _Opts) ->
    {error, cannot_be_undefined};
is_not_undefined(_Args, Value, _Opts) ->
    {ok, Value}.

is_integer([positive], Value, _Opts) when is_integer(Value), Value > 0 ->
    {ok, Value};
is_integer([negative], Value, _Opts) when is_integer(Value), Value < 0 ->
    {ok, Value};
is_integer(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, Value};
is_integer(_Args, _Value, _Opts) ->
    {error, not_integer}.

is_boolean(_Args, Value, _Opts) when is_boolean(Value) ->
    {ok, Value};
is_boolean(_Args, <<"true">> = Value, _Opts) ->
    {ok, Value};
is_boolean(_Args, <<"false">> = Value, _Opts) ->
    {ok, Value};
is_boolean(_Args, <<"1">> = Value, _Opts) ->
    {ok, Value};
is_boolean(_Args, <<"0">> = Value, _Opts) ->
    {ok, Value};
is_boolean(_Args, _Value, _Opts) ->
    {error, not_boolean}.

is_list([not_empty], [], _Opts) ->
    {error, cannot_be_empty};
is_list([empty], [_|_], _Opts) ->
    {error, not_empty};
is_list(_Args, Value, _Opts) when is_list(Value) ->
    {ok, Value};
is_list(_Args, _Value, _Opts) ->
    {error, not_list}.

is_string([not_empty], "", _Opts) ->
    {error, cannot_be_empty};
is_string([empty], [_|_], _Opts) ->
    {error, not_empty};
is_string(_Args, Value, _Opts) when is_list(Value) ->
    case is_char_list(Value) of
        true ->
            {ok, Value};
        false ->
            {error, not_string}
    end;
is_string(_Args, _Value, _Opts) ->
    {error, not_string}.

is_bstring([not_empty], <<"">>, _Opts) ->
    {error, cannot_be_empty};
is_bstring([empty], <<_, _/binary>>, _Opts) ->
    {error, not_empty};
is_bstring(_Args, Value, _Opts) when is_binary(Value) ->
    case is_char_binary(Value) of
        true ->
            {ok, Value};
        false ->
            {error, not_string}
    end;
is_bstring(_Args, _Value, _Opts) ->
    {error, not_bstring}.

is_atom(_Args, Value, _Opts) when is_atom(Value) ->
    {ok, Value};
is_atom(_Args, _Value, _Opts) ->
    {error, not_atom}.

to_integer(_Args, Value, _Opts) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int -> {ok, Int}
    catch
        _:_ -> {error, not_integer}
    end;
to_integer(_Args, Value, _Opts) when is_list(Value) ->
    try list_to_integer(Value) of
        Int -> {ok, Int}
    catch
        _:_ -> {error, not_integer}
    end;
to_integer(_Args, Value, _Opts) when is_float(Value) ->
    try trunc(Value) of
        Int -> {ok, Int}
    catch
        _:_ -> {error, not_integer}
    end;
to_integer(_Args, _Value, _Opts) ->
    {error, not_integer}.

to_boolean(_Args, 0, _Opts) ->
    {ok, false};
to_boolean(_Args, "", _Opts) ->
    {ok, false};
to_boolean(_Args, "0", _Opts) ->
    {ok, false};
to_boolean(_Args, "false", _Opts) ->
    {ok, false};
to_boolean(_Args, <<"">>, _Opts) ->
    {ok, false};
to_boolean(_Args, <<"0">>, _Opts) ->
    {ok, false};
to_boolean(_Args, <<"false">>, _Opts) ->
    {ok, false};
to_boolean(_Args, undefined, _Opts) ->
    {ok, false};
to_boolean(_Args, null, _Opts) ->
    {ok, false};
to_boolean(_Args, _, _Opts) ->
    {ok, true}.

to_string(_Args, Value, _Opts) when is_binary(Value) ->
    try unicode:characters_to_list(Value) of
        Value2 -> {ok, Value2}
    catch
        _:_ -> {error, cant_be_string}
    end;
to_string(_Args, Value,_Opts) when is_atom(Value) ->
    try atom_to_list(Value) of
        Value2 -> {ok, Value2}
    catch
        _:_ -> {error, cant_be_string}
    end;
to_string(_Args, _Value, _Opts) ->
    {error, cant_be_string}.

to_bstring(_Args, Value, _Opts) when is_list(Value) ->
    try unicode:characters_to_binary(Value) of
        Value2 -> {ok, Value2}
    catch
        _:_ -> {error, cant_be_string}
    end;
to_bstring(_Args, Value, _Opts) when is_atom(Value) ->
    try atom_to_binary(Value, utf8) of
        Value2 -> {ok, Value2}
    catch
        _:_ -> {error, cant_be_string}
    end;
to_bstring(_Args, _Value, _Opts) ->
    {error, cant_be_string}.

to_atom(_Args, Value, _Opts) when is_binary(Value) ->
    try binary_to_atom(Value, utf8) of
        Value2 -> {ok, Value2}
    catch
        _:_ -> {error, cant_be_atom}
    end;
to_atom(_Args, Value, _Opts) when is_list(Value) ->
    try list_to_atom(Value) of
        Value2 -> {ok, Value2}
    catch
        _:_ -> {error, cant_be_atom}
    end.

is_proplist(_Args, Value, _Opts) when is_list(Value) ->
    Value2 = [Tuple || {_, _} = Tuple <- Value],
    case Value =:= Value2 of
        true ->
            {ok, Value};
        false ->
            {error, not_proplist}
    end;
is_proplist(_Args, _Value, _Opts) ->
    {error, not_proplist}.

is_map(_Args, Value, _Opts) when is_map(Value) ->
    {ok, Value};
is_map(_Args, _Value, _Opts) ->
    {error, not_map}.

nested_map(Args, Value, Opts) when is_map(Args), is_map(Value) ->
    liver:validate_map(Args, Value, Opts).

nested_proplist(Args, Value, Opts) when is_list(Args), is_list(Value) ->
    liver:validate_map(Args, Value, Opts).

nested_list(Args, Value, Opts) when is_list(Args), is_list(Value) ->
    liver:validate_list(Args, Value, Opts).

%% internal
is_char_list([C|Cs]) when
        is_integer(C), C >= 0, C < 16#D800;
        is_integer(C), C > 16#DFFF, C < 16#FFFE;
        is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    is_char_list(Cs);
is_char_list([]) ->
    true;
is_char_list(_) ->
    false.

is_char_binary(<<C/utf8, Cs/binary>>) when
        is_integer(C), C >= 0, C < 16#D800;
        is_integer(C), C > 16#DFFF, C < 16#FFFE;
        is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    is_char_binary(Cs);
is_char_binary([]) ->
    true;
is_char_binary(_) ->
    false.
