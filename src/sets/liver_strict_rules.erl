-module(liver_strict_rules).

%% API
-export([is_null/3]).
-export([is_undefined/3]).
-export([is_integer/3, to_integer/3]).
-export([is_boolean/3, to_boolean/3]).
-export([is_list/3]).
-export([is_string/3]).
-export([is_bstring/3]).
-export([is_atom/3]).

%% API
is_null(_Args, Value = null, _Opts) ->
    {ok, Value};
is_null(_Args, _Value, _Opts) ->
    {error, not_null}.

is_undefined(_Args, Value = undefined, _Opts) ->
    {ok, Value};
is_undefined(_Args, _Value, _Opts) ->
    {error, not_undefined}.

is_integer(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, Value};
is_integer(_Args, _Value, _Opts) ->
    {error, not_integer}.

is_boolean(_Args, Value, _Opts) when is_boolean(Value) ->
    {ok, Value};
is_boolean(_Args, _Value, _Opts) ->
    {error, not_boolean}.

is_list(_Args, Value, _Opts) when is_list(Value) ->
    {ok, Value};
is_list(_Args, _Value, _Opts) ->
    {error, not_list}.

is_string(_Args, Value, _Opts) when is_list(Value) ->
    case is_char_list(Value) of
        true ->
            {ok, Value};
        false ->
            {error, not_string}
    end;
is_string(_Args, _Value, _Opts) ->
    {error, not_string}.

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
