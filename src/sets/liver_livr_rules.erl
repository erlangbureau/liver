-module(liver_livr_rules).

%% API
%% common rules
-export([required/3]).
-export([not_empty/3]).
-export([not_empty_list/3]).
-export([any_object/3]).

%% string rules
-export([string/3]).
-export([eq/3]).
-export([one_of/3]).
-export([subset/3]).
-export([max_length/3]).
-export([min_length/3]).
-export([length_between/3]).
-export([length_equal/3]).
-export([like/3]).

%% numeric rules
-export([integer/3]).
-export([positive_integer/3]).
-export([decimal/3]).
-export([positive_decimal/3]).
-export([max_number/3]).
-export([min_number/3]).
-export([number_between/3]).

%% special rules
-export([email/3]).
-export([url/3]).
-export([iso_date/3]).
-export([equal_to_field/3]).

%% meta rules
-export([nested_object/3]).
-export([variable_object/3]).
-export([list_of/3]).
-export([list_of_objects/3]).
-export([list_of_different_objects/3]).
-export(['or'/3]).

%% modifiers (previously - "filter rules")
-export([trim/3]).
-export([to_lc/3]).
-export([to_uc/3]).
-export([remove/3]).
-export([leave_only/3]).
-export([default/3]).

-include("liver_rules.hrl").


%% API
%% common rules
required(_Args, <<>>, _Opts) ->
    {error, required};
required(_Args, null, _Opts) ->
    {error, required};
required(_Args, undefined, _Opts) ->
    {error, required};
required(_Args, ?MISSED_FIELD_VALUE, _Opts) ->
    {error, required};
required(_Args, Value, _Opts) ->
    {ok, Value}.

not_empty(_Args, Value, _Opts) ->
    case Value of
        <<>> -> {error, cannot_be_empty};
        _    -> {ok, Value}
    end.

not_empty_list(_Args, Value, _Opts) ->
    case Value of
        <<>>                -> {error, cannot_be_empty};
        [{}]                -> {error, format_error};
        []                  -> {error, cannot_be_empty};
        [_|_]               -> {ok, Value};
        ?MISSED_FIELD_VALUE -> {error, cannot_be_empty};
        _                   -> {error, format_error}
    end.

any_object(_Args, Value, _Opts) ->
    case Value of
        <<>> ->
            {ok, Value};
        #{} ->
            {ok, Value};
        [{}] ->
            {ok, Value};
        [_|_] ->
            case is_proplist(Value) of
                true ->
                    {ok, Value};
                false ->
                    {error, format_error}
            end;
        _ ->
            {error, format_error}
    end.

%% string rules
string(_Args, Value, _Opts) when is_binary(Value) ->
    {ok, Value};
string(_Args, Value, Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    string(_Args, Value2, Opts);
string(_Args, _Value, _Opts) ->
    {error, format_error}.

eq(_Args, <<>>, _Opts) ->
    {ok, <<>>};
eq([Equivalent], Value, Opts) ->
    eq(Equivalent, Value, Opts);
eq(Value, Value, _Opts) ->
    {ok, Value};
%eq(Equivalent, Value, _Opts) when Equivalent == Value ->
%    Value2 = trunc(Value),
%    {ok, Value2};
eq(Equivalent, Value, Opts) when is_binary(Equivalent), is_number(Value) ->
    Value2 = number_to_binary(Value),
    eq(Equivalent, Value2, Opts);
eq(Equivalent, Value, Opts) when is_number(Equivalent), is_binary(Value) ->
    try binary_to_number(Value) of
        Value2 ->
            eq(Equivalent, Value2, Opts)
    catch
        error:badarg ->
            {error, not_allowed_value}
    end;
eq(_Equivalent, Value, _Opts) when is_map(Value); is_list(Value) ->
    {error, format_error};
eq(_Equivalent, _Value, _Opts) ->
    {error, not_allowed_value}.

one_of([List|_], Value, Opts) when is_list(List) ->
    one_of(List, Value, Opts);
one_of([Equivalent|List], Value, Opts) ->
    case eq(Equivalent, Value, Opts) of
        {error, not_allowed_value} ->
            one_of(List, Value, Opts);
        {ok, Value2} ->
            {ok, Value2}
    end;
one_of([], _Value, _Opts) ->
    {error, not_allowed_value};
one_of(Equivalent, Value, Opts) ->
    eq(Equivalent, Value, Opts).

subset(List, Value, Opts) when is_list(Value) ->
    subset(List, Value, Opts, []);
subset(List, Value, Opts) ->
    subset(List, [Value], Opts, []).

subset(List, [Head|Tail], Opts, Acc) ->
    case one_of(List, Head, Opts) of
        {ok, Value} ->
            subset(List, Tail, Opts, [Value|Acc]);
        {error, not_allowed_value} ->
            {error, not_allowed_value}
    end;
subset(_List, [], _Opts, [Acc|[]]) ->
    {ok, Acc};
subset(_List, [], _Opts, Acc) ->
    {ok, lists:reverse(Acc)}.

max_length([MaxLength|_], Value, Opts) ->
    max_length(MaxLength, Value, Opts);
max_length(MaxLength, Value, _Opts) when is_binary(Value) ->
    StrValue = unicode:characters_to_list(Value),
    case length(StrValue) > MaxLength of
        false   -> {ok, Value};
        true    -> {error, too_long}
    end;
max_length(MaxLength, Value, Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    max_length(MaxLength, Value2, Opts);
max_length(_MaxLength, _Value, _Opts) ->
    {error, format_error}.

min_length(_Args, <<>>, _Opts) ->
    {ok, <<>>};
min_length([MinLength|_], Value, _Opts) ->
    min_length(MinLength, Value, _Opts);
min_length(MinLength, Value, _Opts) when is_binary(Value) ->
    StrValue = unicode:characters_to_list(Value),
    case length(StrValue) < MinLength of
        false   -> {ok, Value};
        true    -> {error, too_short}
    end;
min_length(MinLength, Value, _Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    min_length(MinLength, Value2, _Opts);
min_length(_MinLength, _Value, _Opts) ->
    {error, format_error}.

length_between(_Args, <<>>, _Opts) ->
    {ok, <<>>};
length_between([[Min, Max]|_], Value, _Opts) ->
    length_between([Min, Max], Value, _Opts);
length_between([Min, Max|_], Value, _Opts) when is_binary(Value) ->
    StrValue = unicode:characters_to_list(Value),
    Length = length(StrValue),
    if
        Length < Min    -> {error, too_short};
        Length > Max    -> {error, too_long};
        true            -> {ok, Value}
    end;
length_between([Min, Max|_], Value, Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    length_between([Min, Max], Value2, Opts);
length_between(_Args, _Value, _Opts) ->
    {error, format_error}.

length_equal(_Args, <<>>, _Opts) ->
    {ok, <<>>};
length_equal([Length|_], Value, Opts) ->
    length_equal(Length, Value, Opts);
length_equal(Length, Value, _Opts) when is_binary(Value) ->
    StrValue = unicode:characters_to_list(Value),
    ActualLength = length(StrValue),
    if
        ActualLength < Length   -> {error, too_short};
        ActualLength > Length   -> {error, too_long};
        ActualLength == Length  -> {ok, Value}
    end;
length_equal(Length, Value, Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    min_length(Length, Value2, Opts);
length_equal(_Args, _Value, _Opts) ->
    {error, format_error}.

like(_Args, <<>>, _Opts) ->
    {ok, <<>>};
like(Args, Value, _Opts) when is_binary(Value); is_number(Value) ->
    Value2 = to_binary(Value),
    {Pattern, ReOpts} = case Args of
        [RegEx, <<"i">>|_]  -> {RegEx, [unicode, caseless]};
        [RegEx|_]           -> {RegEx, [unicode]};
        RegEx               -> {RegEx, [unicode]}
    end,
    case re:compile(Pattern, ReOpts) of
        {ok, MP} ->
            case re:run(Value2, MP) of
                nomatch -> {error, wrong_format};
                _       -> {ok, Value2}
            end;
        {error, _} ->
            {error, invalid_pattern}
    end;
like(_Args, _Value, _Opts) ->
    {error, format_error}.

%% numeric rules
integer(_Args, <<>>, _Opts) ->
    {ok, <<>>};
integer(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, Value};
integer(_Args, Value, _Opts) when is_binary(Value) ->
    convert(binary_to_integer, Value, not_integer);
integer(_Args, Value, _Opts) when is_float(Value) ->
    {error, not_integer};
integer(_Args, _Value, _Opts) ->
    {error, format_error}.

positive_integer(_Args, <<>>, _Opts) ->
    {ok, <<>>};
positive_integer(_Args, Value, _Opts) when is_integer(Value), Value > 0 ->
    {ok, Value};
positive_integer(_Args, Value, _Opts) when is_binary(Value) ->
    case convert(binary_to_integer, Value, not_positive_integer) of
        {ok, Value2} = OK when Value2 > 0 ->
            OK;
        _Err ->
            {error, not_positive_integer}
    end;
positive_integer(_Args, Value, _Opts) when is_integer(Value); is_float(Value) ->
    {error, not_positive_integer};
positive_integer(_Args, _Value, _Opts) ->
    {error, format_error}.

decimal(_Args, <<>>, _Opts) ->
    {ok, <<>>};
decimal(_Args, Value, _Opts) when is_float(Value) ->
    {ok, Value};
decimal(_Args, Value, _Opts) when is_binary(Value) ->
    try binary_to_number(Value) of
        Value2 ->
            {ok, Value2}
    catch
        _:_ ->
            {error, not_decimal}
    end;
decimal(_Args, Value, _Opts) when is_integer(Value) ->
    {error, not_decimal};
decimal(_Args, _Value, _Opts) ->
    {error, format_error}.

positive_decimal(_Args, <<>>, _Opts) ->
    {ok, <<>>};
positive_decimal(_Args, Value, _Opts) when is_float(Value), Value > 0 ->
    {ok, Value};
positive_decimal(_Args, Value, _Opts) when is_binary(Value) ->
    try binary_to_number(Value) of
        Value2 when Value2 > 0 ->
            {ok, Value2};
        _Err ->
            {error, not_positive_decimal}
    catch
        _:_ ->
            {error, not_positive_decimal}
    end;
positive_decimal(_Args, Value, _Opts) when is_float(Value); is_integer(Value) ->
    {error, not_positive_decimal};
positive_decimal(_Args, _Value, _Opts) ->
    {error, format_error}.

max_number(_Args, <<>>, _Opts) ->
    {ok, <<>>};
max_number([Max|_], Value, Opts) ->
    max_number(Max, Value, Opts);
max_number(Max, Value, _Opts) when is_number(Value) ->
    case Max >= Value of
        true ->
            {ok, Value};
        false ->
            {error, too_high}
    end;
max_number(Max, Value, _Opts) when is_binary(Value) ->
    try binary_to_number(Value) of
        Value2 when Max >= Value2 ->
            {ok, Value2};
        _Value2 ->
            {error, too_high}
    catch
        error:badarg ->
            {error, not_number}
    end;
max_number(_Args, _Value, _Opts) ->
    {error, format_error}.

min_number(_Args, <<>>, _Opts) ->
    {ok, <<>>};
min_number([Min|_], Value, Opts) ->
    min_number(Min, Value, Opts);
min_number(Min, Value, _Opts) when is_number(Value) ->
    case Min =< Value of
        true ->
            {ok, Value};
        false ->
            {error, too_low}
    end;
min_number(Min, Value, _Opts) when is_binary(Value) ->
    try binary_to_number(Value) of
        Value2 when Min =< Value2 ->
            {ok, Value2};
        _Value2 ->
            {error, too_low}
    catch
        error:badarg ->
            {error, not_number}
    end;
min_number(_Args, _Value, _Opts) ->
    {error, format_error}.

number_between(_Args, <<>>, _Opts) ->
    {ok, <<>>};
number_between([[Min, Max]|_], Value, Opts) ->
    number_between([Min, Max], Value, Opts);
number_between([Min, Max|_], Value, _Opts) when is_number(Value) ->
    case {Min =< Value, Value =< Max} of
        {true, true}    -> {ok, Value};
        {false, true}   -> {error, too_low};
        {true, false}   -> {error, too_high}
    end;
number_between([Min, Max|_], Value, _Opts) when is_binary(Value) ->
    try binary_to_number(Value) of
        Value2 when Min =< Value2, Value2 =< Max ->
            {ok, Value2};
        Value2 when Min > Value2 ->
            {error, too_low};
        Value2 when Max < Value2 ->
            {error, too_high}
    catch
        _:_ ->
            {error, not_number}
    end;
number_between(_Args, _Value, _Opts) ->
    {error, format_error}.

%% special rules
%% TODO add support for emails like:
%%  用户@例子.广告              (Chinese, Unicode)
%%  अजय@डाटा.भारत                  (Hindi, Unicode)
%%  квіточка@пошта.укр          (Ukrainian, Unicode)
%%  θσερ@εχαμπλε.ψομ            (Greek, Unicode)
%%  Dörte@Sörensen.example.com  (German, Unicode)
%%  аджай@экзампл.рус           (Russian, Unicode)
email(_Args, <<>> = Value, _Opts) ->
    {ok, Value};
email(_Args, Value, _Opts) when is_binary(Value) ->
    Pattern = "^(([^<>()\\[\\]\\\\.,;:\s@\"]+(\\.[^<>()\\[\\]\\.,;:\s@\"]+)*)"
        "|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])"
        "|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$",
    case re:run(Value, Pattern, [caseless, anchored]) of
        nomatch -> {error, wrong_email};
        _       -> {ok, Value}
    end;
email(_Args, _Value, _Opts) ->
    {error, format_error}.

url(_Args, <<>> = Value, _Opts) ->
    {ok, Value};
url(_Args, Value, _Opts) when is_binary(Value) ->
    Value2 = unicode:characters_to_list(Value),
    Host = case http_uri:parse(Value2) of
        {ok, {http, _UserInfo, Host0, _Port, _Path, _Query}}    -> Host0;
        {ok, {https, _UserInfo, Host0, _Port, _Path, _Query}}   -> Host0;
        _ -> ""
    end,
    Pattern = "^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])"
        "(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9]))*$",
    case re:run(Host, Pattern, [caseless, {capture, none}]) of
        nomatch -> {error, wrong_url};
        _       -> {ok, Value}
    end;
url(_Args, _Value, _Opts) ->
    {error, format_error}.

iso_date(_Args, <<>> = Value, _Opts) ->
    {ok, Value};
iso_date(_Args, <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>>, _Opts) ->
    Date = try
        Year    = binary_to_integer(Y),
        Month   = binary_to_integer(M),
        Day     = binary_to_integer(D),
        {Year, Month, Day}
    catch
        _:_ -> {0, 0, 0}
    end,
    case calendar:valid_date(Date) of
        true ->
            {ok, Date};
        false ->
            {error, wrong_date}
    end;
iso_date(_Args, Value, _Opts) when is_binary(Value) ->
    {error, wrong_date};
iso_date(_Args, _Value, _Opts) ->
    {error, format_error}.

equal_to_field(_Args, <<>> = Value, _Opts) ->
    {ok, Value};
equal_to_field(FieldValue, Value, _Opts)
        when is_binary(Value); is_integer(Value); is_float(Value) ->
    case Value =:= FieldValue of
        true    -> {ok, Value};
        false   -> {error, fields_not_equal}
    end;
equal_to_field(_Args, _Value, _Opts) ->
    {error, format_error}.

%% meta rules
nested_object([List|_], Value, Opts) when is_list(List); is_map(List) ->
    nested_object(List, Value, Opts);
nested_object(Args, Value, Opts) ->
    liver:validate_map(Args, Value, Opts).

variable_object([List|_], Value, Opts) when is_list(List); is_map(List) ->
    variable_object(List, Value, Opts);
variable_object([Field, Schemas|_], Object, Opts) ->
    Type = liver_maps:get(Field, Object, undefined),
    Schema = liver_maps:get(Type, Schemas, undefined),
    liver:validate_map(Schema, Object, Opts).

list_of(_Args, <<>>, _Opts) ->
    {ok, <<>>};
list_of([Rules|_], Value, Opts) when is_list(Rules); is_map(Rules) ->
    list_of(Rules, Value, Opts);
list_of(Rules, ListOfValues, Opts) when is_list(ListOfValues) ->
    liver:validate_list(Rules, ListOfValues, Opts);
list_of(_Args, _Value, _Opts) ->
    {error, format_error}.

list_of_objects(_Args, <<>>, _Opts) ->
    {ok, <<>>};
list_of_objects([List|_], Value, Opts) when is_list(List); is_map(List) ->
    list_of_objects(List, Value, Opts);
list_of_objects(Schema, Objects, Opts) when is_list(Objects) ->
    Results = [liver:validate_map(Schema, Object, Opts) || Object <- Objects],
    case lists:keymember(error, 1, Results) of
        false ->
            ListOfValues2 = [Val || {ok, Val} <- Results],
            {ok, ListOfValues2};
        true ->
            ListOfErrors = [begin
                case Result of
                    {ok, _} -> null;
                    {error, Err} -> Err
                end
            end || Result <- Results],
            {error, ListOfErrors}
    end;
list_of_objects(_Args, _Value, _Opts) ->
    {error, format_error}.

list_of_different_objects(_Args, <<>>, _Opts) ->
    {ok, <<>>};
list_of_different_objects([List|_], Value, Opts) when is_list(List) ->
    list_of_different_objects(List, Value, Opts);
list_of_different_objects(Args, Objects, Opts) when is_list(Objects) ->
    Results = [variable_object(Args, Object, Opts) || Object <- Objects],
    case lists:keymember(error, 1, Results) of
        false ->
            ListOfValues2 = [Val || {ok, Val} <- Results],
            {ok, ListOfValues2};
        true ->
            ListOfErrors = [begin
                case Result of
                    {ok, _} -> null;
                    {error, Err} -> Err
                end
            end || Result <- Results],
            {error, ListOfErrors}
    end;
list_of_different_objects(_Args, _Value, _Opts) ->
    {error, format_error}.

'or'([Rule|Rules], Value, Opts) ->
    case liver:validate_term(Rule, Value, Opts) of
        {ok, _} = Ok ->
            Ok;
        {error, _} = Err ->
            case Rules of
                []  -> Err;
                _   -> 'or'(Rules, Value, Opts)
            end
    end;
'or'([], Value, _Opts) ->
    {ok, Value}.

%% modifiers (previously - "filter rules")
trim(_Args, Value, _Opts) when is_binary(Value) ->
    Value2 = liver_bstring:trim(Value),
    {ok, Value2};
trim(Args, Value, Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    trim(Args, Value2, Opts);
trim(_Args, Value, _Opts) ->
    {ok, Value}.

to_lc(_Args, Value, _Opts) when is_binary(Value) ->
    Value2 = liver_bstring:to_lower(Value),
    {ok, Value2};
to_lc(_Args, Value, _Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    {ok, Value2};
to_lc(_Args, Value, _Opts) ->
    {ok, Value}.

to_uc(_Args, Value, _Opts) when is_binary(Value) ->
    Value2 = liver_bstring:to_upper(Value),
    {ok, Value2};
to_uc(_Args, Value, _Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    {ok, Value2};
to_uc(_Args, Value, _Opts) ->
    {ok, Value}.

remove([Pattern|_], Value, _Opts) ->
    remove(Pattern, Value, _Opts);
remove(Pattern, Value, _Opts) when is_binary(Pattern), is_binary(Value) ->
    Value2 = liver_bstring:remove_chars(Value, Pattern),
    {ok, Value2};
remove(_Args, Value, _Opts) ->
    {ok, Value}.

leave_only([Pattern|_], Value, Opts) ->
    leave_only(Pattern, Value, Opts);
leave_only(Pattern, Value, _Opts) when is_binary(Pattern), is_binary(Value) ->
    Value2 = liver_bstring:leave_chars(Value, Pattern),
    {ok, Value2};
leave_only(_Args, Value, _Opts) ->
    {ok, Value}.

default([{}], _Value, _Opts) ->
    {ok, [{}]};
default([Default], _Value, Opts) ->
    default(Default, _Value, Opts);
default(Default, <<>>, _Opts) ->
    {ok, Default};
default(Default, undefined, _Opts) ->
    {ok, Default};
default(Default, null, _Opts) ->
    {ok, Default};
default(Default, ?MISSED_FIELD_VALUE, _Opts) ->
    {ok, Default};
default(_Default, Value, _Opts) ->
    {ok, Value}.

%% internal
is_proplist([{_, _}|T]) ->
    is_proplist(T);
is_proplist([_|_]) ->
    false;
is_proplist([]) ->
    true.

to_binary(Value) when is_number(Value) ->
    number_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
    Value.

number_to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
number_to_binary(Value) when is_float(Value) ->
    liver_float:to_binary(Value).

binary_to_number(Value) ->
    case binary:match(Value, <<",">>) of
        nomatch ->
            try erlang:binary_to_integer(Value)
            catch
                error:badarg -> erlang:binary_to_float(Value)
            end
    end.

convert(FromTo, Value, Err) ->
    try erlang:FromTo(Value) of
        Value2 -> {ok, Value2}
    catch
        error:badarg -> {error, Err}
    end.
