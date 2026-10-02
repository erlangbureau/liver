-module(liver_standard_rules).

%% Erlang-oriented rules: predicates check types without silent coercion.
%% Use to_* rules when conversion is intentional.

%% presence / nullability
-export([required/3, default/3]).
-export([is_null/3, is_not_null/3]).
-export([is_undefined/3, is_not_undefined/3]).

%% type predicates
-export([is_integer/3, is_non_neg_integer/3, is_pos_integer/3]).
-export([is_float/3, is_number/3]).
-export([is_boolean/3, is_atom/3]).
-export([is_list/3, is_string/3]).
-export([is_utf8_binary/3, is_binary/3]).
-export([is_map/3, is_proplist/3, is_tuple/3]).
-export([is_pid/3, is_ref/3, is_port/3, is_fun/3, is_term/3]).

%% constraints
-export([one_of_terms/3, member/3, range/3]).
-export([byte_size/3, bit_size/3, tuple_size/3, map_size/3, length/3]).

%% converters
-export([to_integer/3, to_float/3, to_boolean/3]).
-export([to_string/3, to_utf8_binary/3, to_binary/3]).
-export([to_atom/3, to_existing_atom/3, to_list/3]).
-export([to_map/3, to_proplist/3]).

%% special
-export([email/3, url/3, iso_date/3]).

%% nested
-export([nested_map/3, nested_list/3, nested_proplist/3]).

-include("liver_rules.hrl").

%%--------------------------------------------------------------------
%% presence / nullability
%%--------------------------------------------------------------------

required(_Args, ?MISSED_FIELD_VALUE, _Opts) ->
    {error, required};
required(_Args, Value, _Opts) ->
    {ok, Value}.

default([Default], Value, Opts) ->
    default(Default, Value, Opts);
default(Default, ?MISSED_FIELD_VALUE, _Opts) ->
    {ok, Default};
default(_Default, Value, _Opts) ->
    {ok, Value}.

is_null(_Args, null, _Opts) ->
    {ok, null};
is_null(_Args, _Value, _Opts) ->
    {error, not_null}.

is_not_null(_Args, null, _Opts) ->
    {error, cannot_be_null};
is_not_null(_Args, Value, _Opts) ->
    {ok, Value}.

is_undefined(_Args, undefined, _Opts) ->
    {ok, undefined};
is_undefined(_Args, _Value, _Opts) ->
    {error, not_undefined}.

is_not_undefined(_Args, undefined, _Opts) ->
    {error, cannot_be_undefined};
is_not_undefined(_Args, Value, _Opts) ->
    {ok, Value}.

%%--------------------------------------------------------------------
%% type predicates
%%--------------------------------------------------------------------

is_integer([positive], Value, _Opts) when is_integer(Value), Value > 0 ->
    {ok, Value};
is_integer([negative], Value, _Opts) when is_integer(Value), Value < 0 ->
    {ok, Value};
is_integer([non_neg], Value, _Opts) when is_integer(Value), Value >= 0 ->
    {ok, Value};
is_integer(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, Value};
is_integer(_Args, _Value, _Opts) ->
    {error, not_integer}.

is_non_neg_integer(_Args, Value, _Opts) when is_integer(Value), Value >= 0 ->
    {ok, Value};
is_non_neg_integer(_Args, _Value, _Opts) ->
    {error, not_non_neg_integer}.

is_pos_integer(_Args, Value, _Opts) when is_integer(Value), Value > 0 ->
    {ok, Value};
is_pos_integer(_Args, _Value, _Opts) ->
    {error, not_pos_integer}.

is_float(_Args, Value, _Opts) when is_float(Value) ->
    {ok, Value};
is_float(_Args, _Value, _Opts) ->
    {error, not_float}.

is_number(_Args, Value, _Opts) when is_number(Value) ->
    {ok, Value};
is_number(_Args, _Value, _Opts) ->
    {error, not_number}.

is_boolean(_Args, Value, _Opts) when is_boolean(Value) ->
    {ok, Value};
is_boolean(_Args, _Value, _Opts) ->
    {error, not_boolean}.

is_atom(_Args, Value, _Opts) when is_atom(Value) ->
    {ok, Value};
is_atom(_Args, _Value, _Opts) ->
    {error, not_atom}.

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
        true -> {ok, Value};
        false -> {error, not_string}
    end;
is_string(_Args, _Value, _Opts) ->
    {error, not_string}.

is_utf8_binary([not_empty], <<>>, _Opts) ->
    {error, cannot_be_empty};
is_utf8_binary([empty], <<_, _/binary>>, _Opts) ->
    {error, not_empty};
is_utf8_binary(_Args, Value, _Opts) when is_binary(Value) ->
    case is_char_binary(Value) of
        true -> {ok, Value};
        false -> {error, not_utf8_binary}
    end;
is_utf8_binary(_Args, _Value, _Opts) ->
    {error, not_utf8_binary}.

is_binary([not_empty], <<>>, _Opts) ->
    {error, cannot_be_empty};
is_binary([empty], <<_, _/binary>>, _Opts) ->
    {error, not_empty};
is_binary(_Args, Value, _Opts) when is_binary(Value) ->
    {ok, Value};
is_binary(_Args, _Value, _Opts) ->
    {error, not_binary}.

is_map(_Args, Value, _Opts) when is_map(Value) ->
    {ok, Value};
is_map(_Args, _Value, _Opts) ->
    {error, not_map}.

is_proplist(_Args, Value, _Opts) when is_list(Value) ->
    case lists:all(fun({_, _}) -> true; (_) -> false end, Value) of
        true -> {ok, Value};
        false -> {error, not_proplist}
    end;
is_proplist(_Args, _Value, _Opts) ->
    {error, not_proplist}.

is_tuple([{size, N}], Value, _Opts) when is_tuple(Value), tuple_size(Value) =:= N ->
    {ok, Value};
is_tuple([{size, _N}], Value, _Opts) when is_tuple(Value) ->
    {error, wrong_tuple_size};
is_tuple(_Args, Value, _Opts) when is_tuple(Value) ->
    {ok, Value};
is_tuple(_Args, _Value, _Opts) ->
    {error, not_tuple}.

is_pid(_Args, Value, _Opts) when is_pid(Value) ->
    {ok, Value};
is_pid(_Args, _Value, _Opts) ->
    {error, not_pid}.

is_ref(_Args, Value, _Opts) when is_reference(Value) ->
    {ok, Value};
is_ref(_Args, _Value, _Opts) ->
    {error, not_ref}.

is_port(_Args, Value, _Opts) when is_port(Value) ->
    {ok, Value};
is_port(_Args, _Value, _Opts) ->
    {error, not_port}.

is_fun([{arity, A}], Value, _Opts) when is_function(Value, A) ->
    {ok, Value};
is_fun([{arity, _A}], Value, _Opts) when is_function(Value) ->
    {error, not_fun};
is_fun(_Args, Value, _Opts) when is_function(Value) ->
    {ok, Value};
is_fun(_Args, _Value, _Opts) ->
    {error, not_fun}.

is_term(_Args, Value, _Opts) ->
    {ok, Value}.

%%--------------------------------------------------------------------
%% constraints
%%--------------------------------------------------------------------

one_of_terms([Allowed], Value, Opts) when is_list(Allowed) ->
    one_of_terms(Allowed, Value, Opts);
one_of_terms(Allowed, Value, _Opts) when is_list(Allowed) ->
    case lists:member(Value, Allowed) of
        true -> {ok, Value};
        false -> {error, not_allowed_value}
    end;
one_of_terms(_Args, _Value, _Opts) ->
    {error, format_error}.

member([Allowed], Value, Opts) when is_list(Allowed) ->
    member(Allowed, Value, Opts);
member(Allowed, Value, _Opts) when is_list(Allowed) ->
    case lists:member(Value, Allowed) of
        true -> {ok, Value};
        false -> {error, not_member}
    end;
member(_Args, _Value, _Opts) ->
    {error, format_error}.

range([{Min, Max}], Value, Opts) ->
    range([Min, Max], Value, Opts);
range([Min, Max], Value, _Opts) when is_number(Value), is_number(Min), is_number(Max) ->
    if
        Value < Min -> {error, too_low};
        Value > Max -> {error, too_high};
        true -> {ok, Value}
    end;
range(_Args, Value, _Opts) when is_number(Value) ->
    {error, format_error};
range(_Args, _Value, _Opts) ->
    {error, not_number}.

byte_size([{eq, N}], Value, _Opts) when is_binary(Value), byte_size(Value) =:= N ->
    {ok, Value};
byte_size([{min, Min}], Value, _Opts) when is_binary(Value), byte_size(Value) >= Min ->
    {ok, Value};
byte_size([{max, Max}], Value, _Opts) when is_binary(Value), byte_size(Value) =< Max ->
    {ok, Value};
byte_size([{between, Min, Max}], Value, _Opts)
  when is_binary(Value), byte_size(Value) >= Min, byte_size(Value) =< Max ->
    {ok, Value};
byte_size(_Args, Value, _Opts) when is_binary(Value) ->
    {error, wrong_byte_size};
byte_size(_Args, _Value, _Opts) ->
    {error, not_binary}.

bit_size([{eq, N}], Value, _Opts) when is_bitstring(Value), bit_size(Value) =:= N ->
    {ok, Value};
bit_size([{min, Min}], Value, _Opts) when is_bitstring(Value), bit_size(Value) >= Min ->
    {ok, Value};
bit_size([{max, Max}], Value, _Opts) when is_bitstring(Value), bit_size(Value) =< Max ->
    {ok, Value};
bit_size(_Args, Value, _Opts) when is_bitstring(Value) ->
    {error, wrong_bit_size};
bit_size(_Args, _Value, _Opts) ->
    {error, not_binary}.

tuple_size([{eq, N}], Value, _Opts) when is_tuple(Value), tuple_size(Value) =:= N ->
    {ok, Value};
tuple_size(_Args, Value, _Opts) when is_tuple(Value) ->
    {error, wrong_tuple_size};
tuple_size(_Args, _Value, _Opts) ->
    {error, not_tuple}.

map_size([{eq, N}], Value, _Opts) when is_map(Value), map_size(Value) =:= N ->
    {ok, Value};
map_size([{min, Min}], Value, _Opts) when is_map(Value), map_size(Value) >= Min ->
    {ok, Value};
map_size([{max, Max}], Value, _Opts) when is_map(Value), map_size(Value) =< Max ->
    {ok, Value};
map_size(_Args, Value, _Opts) when is_map(Value) ->
    {error, wrong_map_size};
map_size(_Args, _Value, _Opts) ->
    {error, not_map}.

length([{eq, N}], Value, _Opts) when is_list(Value), length(Value) =:= N ->
    {ok, Value};
length([{min, Min}], Value, _Opts) when is_list(Value), length(Value) >= Min ->
    {ok, Value};
length([{max, Max}], Value, _Opts) when is_list(Value), length(Value) =< Max ->
    {ok, Value};
length([{between, Min, Max}], Value, _Opts)
  when is_list(Value), length(Value) >= Min, length(Value) =< Max ->
    {ok, Value};
length(_Args, Value, _Opts) when is_list(Value) ->
    {error, wrong_length};
length(_Args, _Value, _Opts) ->
    {error, not_list}.

%%--------------------------------------------------------------------
%% converters
%%--------------------------------------------------------------------

to_integer(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, Value};
to_integer(_Args, Value, _Opts) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int -> {ok, Int}
    catch
        _:_ -> {error, cant_be_integer}
    end;
to_integer(_Args, Value, _Opts) when is_list(Value) ->
    try list_to_integer(Value) of
        Int -> {ok, Int}
    catch
        _:_ -> {error, cant_be_integer}
    end;
to_integer(_Args, Value, _Opts) when is_float(Value) ->
    {ok, trunc(Value)};
to_integer(_Args, _Value, _Opts) ->
    {error, cant_be_integer}.

to_float(_Args, Value, _Opts) when is_float(Value) ->
    {ok, Value};
to_float(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, float(Value)};
to_float(_Args, Value, _Opts) when is_binary(Value) ->
    try binary_to_float(Value) of
        F -> {ok, F}
    catch
        _:_ ->
            try float(binary_to_integer(Value)) of
                F -> {ok, F}
            catch
                _:_ -> {error, cant_be_float}
            end
    end;
to_float(_Args, Value, _Opts) when is_list(Value) ->
    try list_to_float(Value) of
        F -> {ok, F}
    catch
        _:_ ->
            try float(list_to_integer(Value)) of
                F -> {ok, F}
            catch
                _:_ -> {error, cant_be_float}
            end
    end;
to_float(_Args, _Value, _Opts) ->
    {error, cant_be_float}.

to_boolean(_Args, Value, _Opts) when is_boolean(Value) ->
    {ok, Value};
to_boolean(_Args, 0, _Opts) -> {ok, false};
to_boolean(_Args, 1, _Opts) -> {ok, true};
to_boolean(_Args, "", _Opts) -> {ok, false};
to_boolean(_Args, "0", _Opts) -> {ok, false};
to_boolean(_Args, "false", _Opts) -> {ok, false};
to_boolean(_Args, "true", _Opts) -> {ok, true};
to_boolean(_Args, <<>>, _Opts) -> {ok, false};
to_boolean(_Args, <<"0">>, _Opts) -> {ok, false};
to_boolean(_Args, <<"false">>, _Opts) -> {ok, false};
to_boolean(_Args, <<"true">>, _Opts) -> {ok, true};
to_boolean(_Args, undefined, _Opts) -> {ok, false};
to_boolean(_Args, null, _Opts) -> {ok, false};
to_boolean(_Args, _Value, _Opts) ->
    {ok, true}.

to_string(_Args, Value, _Opts) when is_list(Value) ->
    case is_char_list(Value) of
        true -> {ok, Value};
        false -> {error, cant_be_string}
    end;
to_string(_Args, Value, _Opts) when is_binary(Value) ->
    case unicode:characters_to_list(Value) of
        L when is_list(L) -> {ok, L};
        _ -> {error, cant_be_string}
    end;
to_string(_Args, Value, _Opts) when is_atom(Value) ->
    {ok, atom_to_list(Value)};
to_string(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, integer_to_list(Value)};
to_string(_Args, Value, _Opts) when is_float(Value) ->
    {ok, float_to_list(Value, [short])};
to_string(_Args, _Value, _Opts) ->
    {error, cant_be_string}.

to_utf8_binary(_Args, Value, _Opts) when is_binary(Value) ->
    case is_char_binary(Value) of
        true -> {ok, Value};
        false -> {error, cant_be_binary}
    end;
to_utf8_binary(_Args, Value, _Opts) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        B when is_binary(B) -> {ok, B};
        _ -> {error, cant_be_binary}
    end;
to_utf8_binary(_Args, Value, _Opts) when is_atom(Value) ->
    {ok, atom_to_binary(Value, utf8)};
to_utf8_binary(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, integer_to_binary(Value)};
to_utf8_binary(_Args, _Value, _Opts) ->
    {error, cant_be_binary}.

to_binary(_Args, Value, _Opts) when is_binary(Value) ->
    {ok, Value};
to_binary(_Args, Value, _Opts) when is_list(Value) ->
    try iolist_to_binary(Value) of
        B -> {ok, B}
    catch
        _:_ -> {error, cant_be_binary}
    end;
to_binary(_Args, Value, _Opts) when is_atom(Value) ->
    {ok, atom_to_binary(Value, utf8)};
to_binary(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, integer_to_binary(Value)};
to_binary(_Args, _Value, _Opts) ->
    {error, cant_be_binary}.

%% Creates atoms; prefer to_existing_atom for untrusted input.
to_atom(_Args, Value, _Opts) when is_atom(Value) ->
    {ok, Value};
to_atom(_Args, Value, _Opts) when is_binary(Value) ->
    {ok, binary_to_atom(Value, utf8)};
to_atom(_Args, Value, _Opts) when is_list(Value) ->
    try list_to_atom(Value) of
        A -> {ok, A}
    catch
        _:_ -> {error, cant_be_atom}
    end;
to_atom(_Args, _Value, _Opts) ->
    {error, cant_be_atom}.

to_existing_atom(_Args, Value, _Opts) when is_atom(Value) ->
    {ok, Value};
to_existing_atom(_Args, Value, _Opts) when is_binary(Value) ->
    try binary_to_existing_atom(Value, utf8) of
        A -> {ok, A}
    catch
        _:_ -> {error, cant_be_atom}
    end;
to_existing_atom(_Args, Value, _Opts) when is_list(Value) ->
    try list_to_existing_atom(Value) of
        A -> {ok, A}
    catch
        _:_ -> {error, cant_be_atom}
    end;
to_existing_atom(_Args, _Value, _Opts) ->
    {error, cant_be_atom}.

to_list(_Args, Value, _Opts) when is_list(Value) ->
    {ok, Value};
to_list(_Args, Value, _Opts) when is_binary(Value) ->
    {ok, binary_to_list(Value)};
to_list(_Args, Value, _Opts) when is_atom(Value) ->
    {ok, atom_to_list(Value)};
to_list(_Args, Value, _Opts) when is_tuple(Value) ->
    {ok, tuple_to_list(Value)};
to_list(_Args, _Value, _Opts) ->
    {error, cant_be_list}.

to_map(_Args, Value, _Opts) when is_map(Value) ->
    {ok, Value};
to_map(_Args, Value, _Opts) when is_list(Value) ->
    case is_proplist_pairs(Value) of
        true -> {ok, maps:from_list(Value)};
        false -> {error, cant_be_map}
    end;
to_map(_Args, _Value, _Opts) ->
    {error, cant_be_map}.

to_proplist(_Args, Value, _Opts) when is_list(Value) ->
    case is_proplist_pairs(Value) of
        true -> {ok, Value};
        false -> {error, cant_be_proplist}
    end;
to_proplist(_Args, Value, _Opts) when is_map(Value) ->
    {ok, maps:to_list(Value)};
to_proplist(_Args, _Value, _Opts) ->
    {error, cant_be_proplist}.

%%--------------------------------------------------------------------
%% special: email / url / iso_date
%%--------------------------------------------------------------------

%% Accepts Unicode (incl. Cyrillic) local-parts and IDN domains.
email(_Args, Value, _Opts) when is_binary(Value) ->
    case re:run(Value,
                <<"^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$"/utf8>>,
                [anchored, unicode, ucp, {capture, none}]) of
        match -> {ok, Value};
        nomatch -> {error, wrong_email}
    end;
email(_Args, Value, Opts) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        B when is_binary(B) -> email([], B, Opts);
        _ -> {error, wrong_email}
    end;
email(_Args, _Value, _Opts) ->
    {error, format_error}.

url(_Args, Value, _Opts) when is_binary(Value) ->
    case unicode:characters_to_list(Value) of
        List when is_list(List) ->
            case uri_string:parse(List) of
                #{scheme := Scheme, host := Host} when Host =/= "" ->
                    Scheme1 = string:lowercase(Scheme),
                    if
                        Scheme1 =:= "http"; Scheme1 =:= "https" ->
                            {ok, Value};
                        true ->
                            {error, wrong_url}
                    end;
                _ ->
                    {error, wrong_url}
            end;
        _ ->
            {error, wrong_url}
    end;
url(_Args, Value, Opts) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        B when is_binary(B) -> url([], B, Opts);
        _ -> {error, wrong_url}
    end;
url(_Args, _Value, _Opts) ->
    {error, format_error}.

%% Accepts <<"YYYY-MM-DD">> or already-parsed {Y,M,D}.
iso_date(_Args, {Y, M, D} = Date, _Opts)
  when is_integer(Y), is_integer(M), is_integer(D) ->
    case calendar:valid_date(Date) of
        true -> {ok, Date};
        false -> {error, wrong_date}
    end;
iso_date(_Args, <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>>, _Opts) ->
    Date = try
        {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}
    catch
        _:_ -> {0, 0, 0}
    end,
    case calendar:valid_date(Date) of
        true -> {ok, Date};
        false -> {error, wrong_date}
    end;
iso_date(_Args, Value, _Opts) when is_binary(Value) ->
    {error, wrong_date};
iso_date(_Args, _Value, _Opts) ->
    {error, format_error}.

%%--------------------------------------------------------------------
%% nested
%%--------------------------------------------------------------------

nested_map(Args, Value, Opts) when is_map(Args), is_map(Value) ->
    liver:validate_map(Args, Value, Opts);
nested_map(Args, Value, Opts) when is_list(Args), is_list(Value) ->
    liver:validate_map(Args, Value, Opts);
nested_map(_Args, _Value, _Opts) ->
    {error, format_error}.

nested_proplist(Args, Value, Opts) when is_list(Args); is_map(Args) ->
    case Value of
        L when is_list(L) -> liver:validate_map(Args, L, Opts);
        M when is_map(M) -> liver:validate_map(Args, M, Opts);
        _ -> {error, not_proplist}
    end.

nested_list(Args, Value, Opts) when is_list(Value) ->
    liver:validate_list(Args, Value, Opts);
nested_list(_Args, _Value, _Opts) ->
    {error, not_list}.

%%--------------------------------------------------------------------
%% internal
%%--------------------------------------------------------------------

is_proplist_pairs(List) ->
    lists:all(fun({_, _}) -> true; (_) -> false end, List).

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
is_char_binary(<<>>) ->
    true;
is_char_binary(_) ->
    false.
