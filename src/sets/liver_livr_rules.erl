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

%% LIVR 2.5 rules
-export([boolean/3]).
-export([not_one_of/3]).
-export([max_decimals/3]).
-export([digits/3]).
-export([starts_with/3]).
-export([ends_with/3]).
-export([contains/3]).
-export([iso_datetime/3]).
-export([iso_time/3]).
-export([required_if/3]).
-export(['when'/3]).
-export([values/3]).
-export([keys/3]).
-export([any/3]).
-export([list_length/3]).
-export([unique/3]).
-export([split/3]).
-export([uuid/3]).
-export([ip/3]).
-export([cidr/3]).
-export([hostname/3]).
-export([base64/3]).
-export([hex/3]).
-export([digest/3]).
-export([mongo_id/3]).
-export([hex_color/3]).
-export([mac/3]).
-export([e164/3]).
-export([semver/3]).
-export([jwt/3]).
-export([luhn/3]).
-export([credit_card/3]).
-export([iban/3]).

%% schema checks, see liver:check_schema/2
-export([check_args/2]).

-include("liver_rules.hrl").

%% LIVR 2.5: a missing field, null or "" (and undefined, as in required)
-define(IS_EMPTY(Value),
    (Value =:= <<>> orelse Value =:= null orelse Value =:= undefined
     orelse Value =:= ?MISSED_FIELD_VALUE)).

%% LIVR 2.0 rules skip "" and null
-define(IS_NO_VALUE(Value), (Value =:= <<>> orelse Value =:= null)).

-define(IS_PRIMITIVE(Value),
    (is_binary(Value) orelse is_number(Value) orelse is_boolean(Value))).

-define(IS_DIGIT(C), (C >= $0 andalso C =< $9)).
-define(IS_HEX(C), (?IS_DIGIT(C) orelse (C >= $a andalso C =< $f)
                    orelse (C >= $A andalso C =< $F))).
-define(IS_ALPHA(C), ((C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z))).

-define(DAY_MS, 86400000).
-define(MAX_DOUBLE, 1.7976931348623157e308).

%% semver.org 2.0.0, with ASCII classes
-define(SEMVER, "^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)"
    "(-(0|[1-9][0-9]*|[0-9]*[a-zA-Z-][0-9a-zA-Z-]*)"
    "(\\.(0|[1-9][0-9]*|[0-9]*[a-zA-Z-][0-9a-zA-Z-]*))*)?"
    "(\\+[0-9a-zA-Z-]+(\\.[0-9a-zA-Z-]+)*)?$").

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
        null ->
            {ok, Value};
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
string(_Args, null, _Opts) ->
    {ok, null};
string(_Args, Value, _Opts) when is_binary(Value) ->
    {ok, Value};
string(_Args, Value, Opts) when is_number(Value) ->
    Value2 = number_to_binary(Value),
    string(_Args, Value2, Opts);
string(_Args, _Value, _Opts) ->
    {error, format_error}.

eq(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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
%% LIVR 2.5: booleans are compared as strings, so true equals <<"true">>
eq(Equivalent, Value, _Opts) when is_boolean(Equivalent), is_binary(Value) ->
    case atom_to_binary(Equivalent, utf8) of
        Value -> {ok, Value};
        _     -> {error, not_allowed_value}
    end;
eq(Equivalent, Value, _Opts) when is_binary(Equivalent), is_boolean(Value) ->
    case atom_to_binary(Value, utf8) of
        Equivalent  -> {ok, Value};
        _           -> {error, not_allowed_value}
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

max_length(_Args, null, _Opts) ->
    {ok, null};
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

min_length(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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

length_between(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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

length_equal(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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

like(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
like(Args, Value, _Opts) when is_binary(Value); is_number(Value) ->
    Value2 = to_binary(Value),
    {Pattern, ReOpts} = case Args of
        %% dollar_endonly: $ matches only at the end of the value, as in JavaScript,
        %% not before a trailing newline
        [RegEx, <<"i">>|_]  -> {RegEx, [unicode, dollar_endonly, caseless]};
        [RegEx|_]           -> {RegEx, [unicode, dollar_endonly]};
        RegEx               -> {RegEx, [unicode, dollar_endonly]}
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
integer(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
integer(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, Value};
integer(_Args, Value, _Opts) when is_binary(Value) ->
    convert(binary_to_integer, Value, not_integer);
integer(_Args, Value, _Opts) when is_float(Value) ->
    {error, not_integer};
integer(_Args, _Value, _Opts) ->
    {error, format_error}.

positive_integer(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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

decimal(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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
%% an integer is a decimal too, as in LIVR (1 matches its decimal pattern)
decimal(_Args, Value, _Opts) when is_integer(Value) ->
    {ok, Value};
decimal(_Args, _Value, _Opts) ->
    {error, format_error}.

positive_decimal(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
positive_decimal(_Args, Value, _Opts) when is_number(Value), Value > 0 ->
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

max_number(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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

min_number(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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

number_between(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
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
email(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
email(_Args, Value, _Opts) when is_binary(Value) ->
    %% No whitespace (newlines included) outside quotes; dollar_endonly keeps
    %% $ from matching before a trailing newline.
    Pattern = "^(([^<>()\\[\\]\\\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\\\.,;:\\s@\"]+)*)"
        "|(\"[^@\\n\\r]+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])"
        "|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$",
    case re:run(Value, Pattern, [caseless, anchored, dollar_endonly]) of
        nomatch -> {error, wrong_email};
        _       -> {ok, Value}
    end;
email(_Args, _Value, _Opts) ->
    {error, format_error}.


-if(?OTP_RELEASE >= 24).

url(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
url(_Args, Value, _Opts) when is_binary(Value) ->
    Value2 = unicode:characters_to_list(Value),
    X = uri_string:parse(Value2),
    Host = case X of
        #{scheme := Scheme, host := Host0} ->
            Scheme1 = string:lowercase(Scheme),
            if
                Scheme1 == "http"; Scheme1 == "https" ->
                    Host0;
                true ->
                    ""
            end;
        _ ->
            ""
    end,
    Pattern = "^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])"
        "(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9]))*$",
    case re:run(Host, Pattern, [caseless, {capture, none}]) of
        nomatch -> {error, wrong_url};
        _       -> url_port(Value, Value2)
    end;
url(_Args, _Value, _Opts) ->
    {error, format_error}.

-else.

url(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
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
        _       -> url_port(Value, Value2)
    end;
url(_Args, _Value, _Opts) ->
    {error, format_error}.

-endif.

%% LIVR 2.5: a port has 1-5 ASCII digits and a value of 0-65535. The URI
%% parser gives only its value, so the port is read from the authority:
%% after "scheme://" and the user info, up to the path, the query or the
%% fragment. The host has no ":" here, it passed the pattern of url/3.
url_port(Value, Url) ->
    {_Scheme, "://" ++ Rest} = lists:splitwith(fun(C) -> C =/= $: end, Url),
    {Authority, _} = lists:splitwith(fun(C) -> not lists:member(C, "/?#") end, Rest),
    HostPort = lists:last(string:split(Authority, "@", trailing)),
    case lists:splitwith(fun(C) -> C =/= $: end, HostPort) of
        {_Host, ""} ->
            {ok, Value};
        {_Host, ":" ++ Port} ->
            Digits = re:run(Port, "^[0-9]{1,5}$", [dollar_endonly, {capture, none}]),
            case Digits =:= match andalso list_to_integer(Port) =< 65535 of
                true -> {ok, Value};
                false -> {error, wrong_url}
            end
    end.

%% LIVR 2.5: an options object with min and max bounds.
iso_date(Args, Value, _Opts) ->
    {Min, Max} = case Args of
        [] -> {none, none};
        null -> {none, none};
        _ -> parsed(iso_date, Args)
    end,
    if
        ?IS_EMPTY(Value) ->
            {ok, Value};
        is_binary(Value) ->
            case liver_datetime:parse_date(Value) of
                {ok, Date} when Min =:= none, Max =:= none ->
                    {ok, Date};
                {ok, Date} ->
                    Days = calendar:date_to_gregorian_days(Date),
                    in_bounds(Days, date_bound(Min), date_bound(Max), Date,
                              date_too_early, date_too_late);
                error ->
                    {error, wrong_date}
            end;
        ?IS_PRIMITIVE(Value) ->
            {error, wrong_date};
        true ->
            {error, format_error}
    end.

equal_to_field(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
equal_to_field(FieldValue, Value, _Opts)
        when is_binary(Value); is_integer(Value); is_float(Value) ->
    %% LIVR compares the values as strings: <<"10">> equals 10.
    IsEqual = (is_binary(FieldValue) orelse is_number(FieldValue))
        andalso to_binary(FieldValue) =:= to_binary(Value),
    case IsEqual of
        true    -> {ok, Value};
        false   -> {error, fields_not_equal}
    end;
equal_to_field(_Args, _Value, _Opts) ->
    {error, format_error}.

%% meta rules
nested_object([List|_], Value, Opts) when is_list(List); is_map(List) ->
    nested_object(List, Value, Opts);
nested_object(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
nested_object(Args, Value, Opts) ->
    liver:validate_map(Args, Value, Opts).

variable_object([List|_], Value, Opts) when is_list(List); is_map(List) ->
    variable_object(List, Value, Opts);
variable_object(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
variable_object([Field, Schemas|_], Object, Opts) ->
    Type = liver_maps:get(Field, Object, undefined),
    Schema = liver_maps:get(Type, Schemas, undefined),
    liver:validate_map(Schema, Object, Opts).

list_of(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
list_of([Rules|_], Value, Opts) when is_list(Rules); is_map(Rules) ->
    list_of(Rules, Value, Opts);
list_of(_Rules, [{_, _}|_], _Opts) ->
    %% a proplist is an object
    {error, format_error};
list_of(Rules, ListOfValues, Opts) when is_list(ListOfValues), ListOfValues =/= [{}] ->
    liver:validate_list(Rules, ListOfValues, Opts);
list_of(_Args, _Value, _Opts) ->
    {error, format_error}.

list_of_objects(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
list_of_objects([List|_], Value, Opts) when is_list(List); is_map(List) ->
    list_of_objects(List, Value, Opts);
list_of_objects(_Args, [{}], _Opts) ->
    %% an empty object
    {error, format_error};
list_of_objects(_Args, [{_, _}|_], _Opts) ->
    %% a proplist is an object
    {error, format_error};
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

list_of_different_objects(_Args, Value, _Opts) when ?IS_NO_VALUE(Value) ->
    {ok, Value};
list_of_different_objects([List|_], Value, Opts) when is_list(List) ->
    list_of_different_objects(List, Value, Opts);
list_of_different_objects(_Args, [{}], _Opts) ->
    %% an empty object
    {error, format_error};
list_of_different_objects(_Args, [{_, _}|_], _Opts) ->
    %% a proplist is an object
    {error, format_error};
list_of_different_objects(Args, Objects, Opts) when is_list(Objects) ->
    Results = [case Object of
                   _ when ?IS_NO_VALUE(Object) -> {error, liver:error_code(format_error, Opts)};
                   _ -> variable_object(Args, Object, Opts)
               end || Object <- Objects],
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

%% LIVR 2.5 rules
%% types and numbers
boolean(Args, Value, _Opts) ->
    parsed(boolean, Args),
    if
        ?IS_EMPTY(Value) ->
            {ok, Value};
        ?IS_PRIMITIVE(Value) ->
            case string_form(Value) of
                <<"true">>  -> {ok, true};
                <<"1">>     -> {ok, true};
                <<"false">> -> {ok, false};
                <<"0">>     -> {ok, false};
                _           -> {error, not_boolean}
            end;
        true ->
            {error, format_error}
    end.

not_one_of(Args, Value, _Opts) ->
    Forbidden = parsed(not_one_of, Args),
    if
        ?IS_EMPTY(Value) ->
            {ok, Value};
        ?IS_PRIMITIVE(Value) ->
            case lists:member(string_form(Value), Forbidden) of
                true    -> {error, not_allowed_value};
                false   -> {ok, Value}
            end;
        true ->
            {error, format_error}
    end.

max_decimals(Args, Value, _Opts) ->
    Max = parsed(max_decimals, Args),
    if
        ?IS_EMPTY(Value) ->
            {ok, Value};
        is_number(Value) ->
            case decimals(string_form(Value)) =< Max of
                true    -> {ok, Value};
                false   -> {error, too_many_decimals}
            end;
        is_binary(Value) ->
            %% "1.230" has 2 decimals, the output is a number
            case decimal_string_to_number(Value) of
                {ok, Number} ->
                    case decimals(Value) =< Max of
                        true    -> {ok, Number};
                        false   -> {error, too_many_decimals}
                    end;
                error ->
                    {error, not_number}
            end;
        is_boolean(Value) ->
            {error, not_number};
        true ->
            {error, format_error}
    end.

%% strings
digits(Args, Value, _Opts) ->
    parsed(digits, Args),
    check_string(Value, not_digits, fun all_digits/1).

starts_with(Args, Value, _Opts) ->
    Prefixes = parsed(starts_with, Args),
    check_string(Value, wrong_format, fun(String) ->
        lists:any(fun(Prefix) ->
            binary:longest_common_prefix([String, Prefix]) =:= byte_size(Prefix)
        end, Prefixes)
    end).

ends_with(Args, Value, _Opts) ->
    Suffixes = parsed(ends_with, Args),
    check_string(Value, wrong_format, fun(String) ->
        lists:any(fun(Suffix) ->
            binary:longest_common_suffix([String, Suffix]) =:= byte_size(Suffix)
        end, Suffixes)
    end).

contains(Args, Value, _Opts) ->
    Parts = parsed(contains, Args),
    check_string(Value, wrong_format, fun(String) ->
        binary:match(String, Parts) =/= nomatch
    end).

%% dates and times (iso_date is above)
iso_datetime(Args, Value, _Opts) ->
    {Min, Max} = parsed(iso_datetime, Args),
    if
        ?IS_EMPTY(Value) ->
            {ok, Value};
        is_binary(Value) ->
            case liver_datetime:parse_datetime(Value) of
                {ok, Ms} ->
                    case liver_datetime:is_writable(Ms) of
                        true ->
                            in_bounds(Ms, datetime_bound(Min, min), datetime_bound(Max, max),
                                      liver_datetime:format_datetime(Ms),
                                      datetime_too_early, datetime_too_late);
                        false ->
                            {error, wrong_datetime}
                    end;
                error ->
                    {error, wrong_datetime}
            end;
        ?IS_PRIMITIVE(Value) ->
            {error, wrong_datetime};
        true ->
            {error, format_error}
    end.

iso_time(Args, Value, _Opts) ->
    {Min, Max} = parsed(iso_time, Args),
    if
        ?IS_EMPTY(Value) ->
            {ok, Value};
        ?IS_PRIMITIVE(Value) ->
            String = string_form(Value),
            case liver_datetime:parse_time(String) of
                {ok, Time} -> time_in_bounds(Time, Min, Max, String);
                error -> {error, wrong_time}
            end;
        true ->
            {error, format_error}
    end.

%% presence
required_if(Args, Value, Opts) ->
    Conditions = parsed(required_if, Args),
    IsRequired = ?IS_EMPTY(Value) andalso lists:all(fun({Ref, Allowed}) ->
        case reference_value(Ref, Opts) of
            {ok, RefValue} when ?IS_PRIMITIVE(RefValue), RefValue =/= <<>> ->
                lists:member(string_form(RefValue), Allowed);
            _ ->
                false
        end
    end, Conditions),
    case IsRequired of
        true    -> {error, required};
        false   -> {ok, Value}
    end.

%% meta rules
'when'(Args, Value, Opts) ->
    case parsed('when', Args) of
        {condition, Ref, Condition, IfTrue, IfFalse} ->
            IsTrue = case reference_value(Ref, Opts) of
                {ok, RefValue} when not ?IS_EMPTY(RefValue) ->
                    %% the output of the condition rules is discarded
                    element(1, liver:validate_term(Condition, RefValue, Opts)) =:= ok;
                _ ->
                    false
            end,
            case {IsTrue, IfFalse} of
                {true, _}       -> liver:validate_term(IfTrue, Value, Opts);
                {false, none}   -> {ok, Value};
                {false, _}      -> liver:validate_term(IfFalse, Value, Opts)
            end;
        {switch, Ref, Cases} ->
            Case = case reference_value(Ref, Opts) of
                {ok, RefValue} when ?IS_PRIMITIVE(RefValue), RefValue =/= <<>> ->
                    lists:keyfind(string_form(RefValue), 1, Cases);
                _ ->
                    false
            end,
            case Case of
                {_Key, Rules} ->
                    liver:validate_term(Rules, Value, Opts);
                false when ?IS_EMPTY(Value) ->
                    {ok, Value};
                false ->
                    {error, not_allowed_value}
            end
    end.

values(Args, Value, Opts) ->
    Rules = parsed(values, Args),
    case is_object(Value) of
        true ->
            ItemOpts = Opts#{?CONTAINER_OPT => #{}},
            Results = [{K, liver:validate_term(Rules, V, ItemOpts)}
                       || {K, V} <- object_to_list(Value)],
            case [{K, Err} || {K, {error, Err}} <- Results] of
                []      -> {ok, object_like([{K, V} || {K, {ok, V}} <- Results], Value)};
                Errors  -> {error, object_like(Errors, Value)}
            end;
        false when ?IS_EMPTY(Value) ->
            {ok, Value};
        false ->
            {error, format_error}
    end.

keys(Args, Value, Opts) ->
    Rules = parsed(keys, Args),
    case is_object(Value) of
        true ->
            ItemOpts = Opts#{?CONTAINER_OPT => #{}},
            Errors = [{K, liver:error_code(wrong_key, Opts)}
                      || {K, _V} <- object_to_list(Value),
                         element(1, liver:validate_term(Rules, key_string(K), ItemOpts)) =:= error],
            case Errors of
                []  -> {ok, Value};
                _   -> {error, object_like(Errors, Value)}
            end;
        false when ?IS_EMPTY(Value) ->
            {ok, Value};
        false ->
            {error, format_error}
    end.

any(Args, Value, _Opts) ->
    parsed(any, Args),
    {ok, Value}.

%% lists
list_length(Args, Value, _Opts) ->
    {Min, Max} = parsed(list_length, Args),
    case is_list(Value) andalso not is_object(Value) of
        true when length(Value) < Min   -> {error, too_few_items};
        true when length(Value) > Max   -> {error, too_many_items};
        true                            -> {ok, Value};
        false when ?IS_EMPTY(Value)     -> {ok, Value};
        false                           -> {error, format_error}
    end.

unique(Args, Value, _Opts) ->
    Keys = parsed(unique, Args),
    case is_list(Value) andalso not is_object(Value) of
        true ->
            try [Item || V <- Value, Item <- [unique_item(V, Keys)], Item =/= skip] of
                Items ->
                    case length(lists:usort(Items)) =:= length(Items) of
                        true    -> {ok, Value};
                        false   -> {error, not_unique_items}
                    end
            catch
                throw:format_error -> {error, format_error}
            end;
        false when ?IS_EMPTY(Value) ->
            {ok, Value};
        false ->
            {error, format_error}
    end.

%% modifiers
split(Args, Value, _Opts) ->
    Separator = parsed(split, Args),
    case Value of
        <<>> ->
            {ok, Value};
        _ when ?IS_PRIMITIVE(Value) ->
            {ok, binary:split(string_form(Value), Separator, [global])};
        _ ->
            {ok, Value}
    end.

%% formats
uuid(Args, Value, _Opts) ->
    Versions = parsed(uuid, Args),
    check_format(Value, wrong_uuid, fun(String) -> is_uuid(String, Versions) end).

ip(Args, Value, _Opts) ->
    Version = parsed(ip, Args),
    check_format(Value, wrong_ip, fun(String) -> ip_version(String, Version) =/= false end).

cidr(Args, Value, _Opts) ->
    Version = parsed(cidr, Args),
    check_format(Value, wrong_cidr, fun(String) ->
        case binary:split(String, <<"/">>) of
            [Ip, Prefix] ->
                case ip_version(Ip, Version) of
                    v4      -> is_prefix(Prefix, 32);
                    v6      -> is_prefix(Prefix, 128);
                    false   -> false
                end;
            _ ->
                false
        end
    end).

hostname(Args, Value, _Opts) ->
    parsed(hostname, Args),
    check_format(Value, wrong_hostname, fun is_hostname/1).

base64(Args, Value, _Opts) ->
    Alphabet = parsed(base64, Args),
    check_format(Value, wrong_base64, fun(String) -> is_base64(String, Alphabet) end).

hex(Args, Value, _Opts) ->
    parsed(hex, Args),
    check_format(Value, wrong_hex, fun is_hex/1).

digest(Args, Value, _Opts) ->
    Lengths = parsed(digest, Args),
    check_format(Value, wrong_digest, fun(String) ->
        lists:member(byte_size(String), Lengths) andalso is_hex(String)
    end).

mongo_id(Args, Value, _Opts) ->
    parsed(mongo_id, Args),
    check_format(Value, wrong_mongo_id, fun(String) ->
        byte_size(String) =:= 24 andalso is_hex(String)
    end).

hex_color(Args, Value, _Opts) ->
    parsed(hex_color, Args),
    check_format(Value, wrong_hex_color, fun
        (<<"#", Hex/binary>>) ->
            lists:member(byte_size(Hex), [3, 4, 6, 8]) andalso is_hex(Hex);
        (_) ->
            false
    end).

mac(Args, Value, _Opts) ->
    Pairs = parsed(mac, Args),
    check_format(Value, wrong_mac, fun(String) ->
        lists:any(fun(Separator) ->
            Parts = binary:split(String, Separator, [global]),
            length(Parts) =:= Pairs andalso
                lists:all(fun(Part) -> byte_size(Part) =:= 2 andalso is_hex(Part) end, Parts)
        end, [<<":">>, <<"-">>])
    end).

e164(Args, Value, _Opts) ->
    parsed(e164, Args),
    check_format(Value, wrong_e164, fun
        (<<"+", First, Rest/binary>>) when First >= $1, First =< $9,
                byte_size(Rest) >= 6, byte_size(Rest) =< 14 ->
            all_digits(Rest);
        (_) ->
            false
    end).

semver(Args, Value, _Opts) ->
    parsed(semver, Args),
    check_format(Value, wrong_semver, fun(String) ->
        re:run(String, ?SEMVER, [dollar_endonly, {capture, none}]) =:= match
    end).

jwt(Args, Value, _Opts) ->
    parsed(jwt, Args),
    check_format(Value, wrong_jwt, fun(String) ->
        case binary:split(String, <<".">>, [global]) of
            [Header, Payload, _Signature] = Segments when Header =/= <<>>, Payload =/= <<>> ->
                lists:all(fun(Segment) ->
                    byte_size(Segment) rem 4 =/= 1 andalso is_base64_chars(Segment, url)
                end, Segments);
            _ ->
                false
        end
    end).

%% checksums
luhn(Args, Value, _Opts) ->
    parsed(luhn, Args),
    check_format(Value, wrong_checksum, fun(String) ->
        byte_size(String) >= 2 andalso is_luhn(String)
    end).

credit_card(Args, Value, _Opts) ->
    parsed(credit_card, Args),
    check_format(Value, wrong_credit_card_number, fun(String) ->
        byte_size(String) >= 12 andalso byte_size(String) =< 19 andalso is_luhn(String)
    end).

iban(Args, Value, _Opts) ->
    parsed(iban, Args),
    check_format(Value, wrong_iban, fun is_iban/1).

%% schema checks
%% liver:check_schema/2 calls it for every rule of the schema: ok,
%% {ok, Nested} with the rules and the schemas in the arguments, or error.
check_args(Rule, Args) ->
    try args(Rule, Args) of
        Parsed -> nested(Rule, Parsed)
    catch
        throw:invalid_args -> error
    end.

nested(Rule, Schema) when Rule =:= nested_object; Rule =:= list_of_objects ->
    {ok, [{schema, Schema}]};
nested(Rule, Schemas) when Rule =:= variable_object; Rule =:= list_of_different_objects ->
    {ok, [{schema, Schema} || Schema <- Schemas]};
nested(Rule, Rules) when Rule =:= list_of; Rule =:= values; Rule =:= keys ->
    {ok, [{rules, Rules}]};
nested('or', Branches) ->
    {ok, [{rules, Rules} || Rules <- Branches]};
nested('when', {condition, _Ref, Condition, IfTrue, IfFalse}) ->
    {ok, [{rules, Rules} || Rules <- [Condition, IfTrue, IfFalse], Rules =/= none]};
nested('when', {switch, _Ref, Cases}) ->
    {ok, [{rules, Rules} || {_Key, Rules} <- Cases]};
nested(_Rule, _Parsed) ->
    ok.

%% The arguments of a rule (LIVR 2.5, 2.6), or throw(invalid_args).
%% The LIVR 2.0 rules check them only in check_args/2.
parsed(Rule, Args) ->
    try args(Rule, Args)
    catch
        throw:invalid_args -> error({invalid_args, Rule, Args})
    end.

%% {"rule": null} is one null argument (2.6): a schema error for the 2.5
%% rules. LIVR 2.0 ignored the arguments of iso_date, so {"iso_date": null}
%% stays the plain iso_date. The other 2.0 rules and custom rules get null as
%% their argument, so {"default": null} sets null.
args(iso_date, null) ->
    {none, none};
args(Rule, Args) when Rule =:= boolean; Rule =:= digits; Rule =:= any;
        Rule =:= hostname; Rule =:= hex; Rule =:= mongo_id; Rule =:= hex_color;
        Rule =:= e164; Rule =:= semver; Rule =:= jwt; Rule =:= luhn;
        Rule =:= credit_card; Rule =:= iban ->
    case Args of
        [] -> none;
        _ -> throw(invalid_args)
    end;
args(not_one_of, Args) ->
    non_empty([primitive_arg(Arg) || Arg <- args_list(Args)]);
args(max_decimals, Args) ->
    case args_list(Args) of
        [Max|_] -> count_arg(Max);
        [] -> throw(invalid_args)
    end;
args(Rule, Args) when Rule =:= starts_with; Rule =:= ends_with; Rule =:= contains ->
    non_empty([non_empty_string_arg(Arg) || Arg <- args_list(Args)]);
args(iso_date, Args) ->
    ordered(bound_options(Args, fun date_bound_arg/2));
args(iso_datetime, Args) ->
    ordered(bound_options(Args, fun datetime_bound_arg/2));
args(iso_time, Args) ->
    bound_options(Args, fun time_bound_arg/2);
args(required_if, Args) ->
    case args_list(Args) of
        [Conditions|_] ->
            case is_object(Conditions) of
                true ->
                    non_empty([{reference(Ref), allowed_values(Allowed)}
                               || {Ref, Allowed} <- object_to_list(Conditions)]);
                false ->
                    throw(invalid_args)
            end;
        [] ->
            throw(invalid_args)
    end;
args('when', Args) ->
    case args_list(Args) of
        [Ref, Cases] ->
            {switch, reference(Ref), cases(Cases)};
        [Ref, Condition, IfTrue] ->
            {condition, reference(Ref), rules(Condition), rules(IfTrue), none};
        [Ref, Condition, IfTrue, IfFalse|_] ->
            {condition, reference(Ref), rules(Condition), rules(IfTrue), rules(IfFalse)};
        _ ->
            throw(invalid_args)
    end;
args(Rule, Args) when Rule =:= values; Rule =:= keys ->
    %% as in list_of: [Rule1, Rule2] or [[Rule1, Rule2]]
    case Args of
        [Rules] when is_list(Rules) -> rules(Rules);
        _ -> rules(Args)
    end;
args(list_length, Args) ->
    case args_list(Args) of
        [] ->
            throw(invalid_args);
        [First|Rest] ->
            {Min, Max} = case {is_object(First), Rest} of
                {true, _} ->
                    case object_to_list(First) of
                        [] -> throw(invalid_args);
                        Options -> lists:foldl(fun({K, V}, {Min0, Max0}) ->
                            case key_string(K) of
                                <<"min">> -> {count_arg(V), Max0};
                                <<"max">> -> {Min0, count_arg(V)};
                                _ -> throw(invalid_args)
                            end
                        end, {0, infinity}, Options)
                    end;
                {false, []} ->
                    Length = count_arg(First),
                    {Length, Length};
                {false, [Max0|_]} ->
                    {count_arg(First), count_arg(Max0)}
            end,
            %% integers are smaller than atoms: Min > infinity is false
            case Min > Max of
                true -> throw(invalid_args);
                false -> {Min, Max}
            end
    end;
args(unique, Args) ->
    [key_arg(Key) || Key <- args_list(Args)];
args(split, Args) ->
    case args_list(Args) of
        [Separator|_] -> non_empty_string_arg(Separator);
        [] -> throw(invalid_args)
    end;
args(uuid, Args) ->
    case args_list(Args) of
        [] -> any;
        Versions -> [uuid_version(string_arg(Version)) || Version <- Versions]
    end;
args(Rule, Args) when Rule =:= ip; Rule =:= cidr ->
    case args_list(Args) of
        [] -> any;
        [Version|_] ->
            case string_arg(Version) of
                <<"v4">> -> v4;
                <<"v6">> -> v6;
                _ -> throw(invalid_args)
            end
    end;
args(base64, Args) ->
    case args_list(Args) of
        [] -> standard;
        [Alphabet|_] ->
            case string_arg(Alphabet) of
                <<"url">> -> url;
                _ -> throw(invalid_args)
            end
    end;
args(digest, Args) ->
    non_empty([digest_length(string_arg(Algorithm)) || Algorithm <- args_list(Args)]);
args(mac, Args) ->
    case args_list(Args) of
        [] -> 6;
        [Bits|_] ->
            case string_arg(Bits) of
                <<"48">> -> 6;
                <<"64">> -> 8;
                _ -> throw(invalid_args)
            end
    end;
%% LIVR 2.0 rules: only the rules in the arguments are checked
args(Rule, [Schema|_]) when (Rule =:= nested_object orelse Rule =:= list_of_objects
                             orelse Rule =:= list_of),
                            (is_list(Schema) orelse is_map(Schema)) ->
    Schema;
args(Rule, Schema) when Rule =:= nested_object; Rule =:= list_of_objects; Rule =:= list_of ->
    Schema;
args('or', Branches) when is_list(Branches) ->
    Branches;
args(variable_object, [Args|_]) when is_list(Args); is_map(Args) ->
    args(variable_object, Args);
args(list_of_different_objects, [Args|_]) when is_list(Args) ->
    args(list_of_different_objects, Args);
args(Rule, Args) when Rule =:= variable_object; Rule =:= list_of_different_objects ->
    case Args of
        [_Field, Schemas|_] ->
            case is_object(Schemas) of
                true -> [Schema || {_Name, Schema} <- object_to_list(Schemas)];
                false -> throw(invalid_args)
            end;
        _ ->
            throw(invalid_args)
    end;
args(like, Args) ->
    {Pattern, ReOpts} = case Args of
        [RegEx, <<"i">>|_]  -> {RegEx, [unicode, dollar_endonly, caseless]};
        [RegEx|_]           -> {RegEx, [unicode, dollar_endonly]};
        RegEx               -> {RegEx, [unicode, dollar_endonly]}
    end,
    case is_binary(Pattern) andalso re:compile(Pattern, ReOpts) of
        {ok, _} -> none;
        _ -> throw(invalid_args)
    end;
args(_Rule, _Args) ->
    none.

%% internal (LIVR 2.5)

%% The string form of a primitive value (2.2): integral numbers without a
%% decimal point, other numbers as the shortest decimal that reads back
%% as the same double, without an exponent.
string_form(Value) when is_binary(Value) ->
    Value;
string_form(true) ->
    <<"true">>;
string_form(false) ->
    <<"false">>;
string_form(Value) when is_integer(Value) ->
    integer_to_binary(Value);
string_form(Value) when is_float(Value), Value == trunc(Value) ->
    integer_to_binary(trunc(Value));
string_form(Value) when is_float(Value) ->
    %% shortest round trip (OTP 25+), e.g. "0.1" or "1.5e-7"
    case string:split(float_to_list(Value, [short]), "e") of
        [Decimal] -> list_to_binary(Decimal);
        [Mantissa, Exponent] -> expand_exponent(Mantissa, list_to_integer(Exponent))
    end.

expand_exponent("-" ++ Mantissa, Exponent) ->
    <<"-", (expand_exponent(Mantissa, Exponent))/binary>>;
expand_exponent(Mantissa, Exponent) ->
    [Int, Frac] = string:split(Mantissa, "."),
    Digits = string:trim(Int ++ Frac, leading, "0"),
    Point = length(Int) + Exponent - (length(Int ++ Frac) - length(Digits)),
    Decimal = if
        Point =< 0 -> "0." ++ lists:duplicate(-Point, $0) ++ Digits;
        Point >= length(Digits) -> Digits ++ lists:duplicate(Point - length(Digits), $0);
        true -> lists:sublist(Digits, Point) ++ "." ++ lists:nthtail(Point, Digits)
    end,
    list_to_binary(string:trim(string:trim(Decimal, trailing, "0"), trailing, ".")).

%% Digits after the decimal point, without trailing zeros.
decimals(Number) ->
    case binary:split(Number, <<".">>) of
        [_Int, Frac] -> string:length(string:trim(Frac, trailing, "0"));
        [_Int] -> 0
    end.

%% -?[0-9]+(\.[0-9]+)?, a finite double -> {ok, Number} | error
decimal_string_to_number(Value) ->
    Unsigned = case Value of
        <<"-", Rest/binary>> -> Rest;
        _ -> Value
    end,
    case binary:split(Unsigned, <<".">>) of
        [Int] ->
            case all_digits(Int) of
                true ->
                    Integer = binary_to_integer(Value),
                    case abs(Integer) =< ?MAX_DOUBLE of
                        true -> {ok, Integer};
                        false -> error
                    end;
                false ->
                    error
            end;
        [Int, Frac] ->
            case all_digits(Int) andalso all_digits(Frac) of
                true ->
                    try {ok, binary_to_float(Value)}
                    catch error:badarg -> error
                    end;
                false ->
                    error
            end
    end.

%% String rules: the string form of any primitive value.
check_string(Value, _Code, _IsValid) when ?IS_EMPTY(Value) ->
    {ok, Value};
check_string(Value, Code, IsValid) when ?IS_PRIMITIVE(Value) ->
    String = string_form(Value),
    case IsValid(String) of
        true    -> {ok, String};
        false   -> {error, Code}
    end;
check_string(_Value, _Code, _IsValid) ->
    {error, format_error}.

%% Format and checksum rules: as check_string/3, but a boolean is never
%% valid (true is not a hostname, although <<"true">> is).
check_format(Value, Code, _IsValid) when is_boolean(Value) ->
    {error, Code};
check_format(Value, Code, IsValid) ->
    check_string(Value, Code, IsValid).

in_bounds(Value, Min, _Max, _Out, TooEarly, _TooLate) when Min =/= none, Value < Min ->
    {error, TooEarly};
in_bounds(Value, _Min, Max, _Out, _TooEarly, TooLate) when Max =/= none, Value > Max ->
    {error, TooLate};
in_bounds(_Value, _Min, _Max, Out, _TooEarly, _TooLate) ->
    {ok, Out}.

%% A bound in Gregorian days.
date_bound(none) -> none;
date_bound({fixed, Days}) -> Days;
date_bound({relative, Days}) -> liver_datetime:today() + Days.

%% A bound in milliseconds, see liver_datetime:parse_datetime/1.
datetime_bound(none, _MinMax) -> none;
datetime_bound({fixed, Ms}, _MinMax) -> Ms;
datetime_bound({relative, Days}, min) -> (liver_datetime:today() + Days) * ?DAY_MS;
datetime_bound({relative, Days}, max) -> (liver_datetime:today() + Days + 1) * ?DAY_MS - 1.

%% min > max is a range over midnight: a time in the gap gets the error of
%% the nearer bound, too early in the middle.
time_in_bounds(Time, Min, Max, Out) when Min =/= none, Max =/= none, Min > Max ->
    if
        Time >= Min; Time =< Max -> {ok, Out};
        Time - Max < Min - Time -> {error, time_too_late};
        true -> {error, time_too_early}
    end;
time_in_bounds(Time, Min, Max, Out) ->
    in_bounds(Time, Min, Max, Out, time_too_early, time_too_late).

%% {"min": Bound, "max": Bound} -> {Min, Max}
bound_options(Args, ParseBound) ->
    case args_list(Args) of
        [] ->
            {none, none};
        [Options|_] ->
            case is_object(Options) of
                true ->
                    lists:foldl(fun({K, V}, {Min, Max}) ->
                        case key_string(K) of
                            <<"min">> -> {ParseBound(V, min), Max};
                            <<"max">> -> {Min, ParseBound(V, max)};
                            _ -> throw(invalid_args)
                        end
                    end, {none, none}, object_to_list(Options));
                false ->
                    throw(invalid_args)
            end
    end.

%% min later than max: no value could pass. A fixed and a relative bound
%% are not compared, the result depends on the day.
ordered({{Kind, Min}, {Kind, Max}}) when Min > Max ->
    throw(invalid_args);
ordered(Bounds) ->
    Bounds.

date_bound_arg(<<"today">>, _MinMax) -> {relative, 0};
date_bound_arg(<<"yesterday">>, _MinMax) -> {relative, -1};
date_bound_arg(<<"tomorrow">>, _MinMax) -> {relative, 1};
date_bound_arg(Bound, _MinMax) when is_binary(Bound) ->
    case liver_datetime:parse_date(Bound) of
        {ok, Date} -> {fixed, calendar:date_to_gregorian_days(Date)};
        error -> throw(invalid_args)
    end;
date_bound_arg(_Bound, _MinMax) ->
    throw(invalid_args).

datetime_bound_arg(Bound, MinMax) when is_binary(Bound) ->
    case liver_datetime:parse_datetime(Bound) of
        {ok, Ms} ->
            {fixed, Ms};
        error ->
            %% a date: the start or the end of that day
            case date_bound_arg(Bound, MinMax) of
                {fixed, Days} when MinMax =:= min -> {fixed, Days * ?DAY_MS};
                {fixed, Days} -> {fixed, (Days + 1) * ?DAY_MS - 1};
                Relative -> Relative
            end
    end;
datetime_bound_arg(_Bound, _MinMax) ->
    throw(invalid_args).

time_bound_arg(Bound, _MinMax) when is_binary(Bound) ->
    case liver_datetime:parse_time(Bound) of
        {ok, Time} -> Time;
        error -> throw(invalid_args)
    end;
time_bound_arg(_Bound, _MinMax) ->
    throw(invalid_args).

%% A field reference (2.5): "name", "a/b/c", ~1 for / and ~0 for ~.
%% An atom is a field name of a schema with atom keys.
reference(Ref) when is_atom(Ref), Ref =/= null, Ref =/= undefined, not is_boolean(Ref) ->
    [Ref];
reference(Ref) when is_binary(Ref), Ref =/= <<>> ->
    [unescape(Segment, <<>>) || Segment <- binary:split(Ref, <<"/">>, [global])];
reference(_Ref) ->
    throw(invalid_args).

unescape(<<>>, <<>>) -> throw(invalid_args);
unescape(<<"~0", Rest/binary>>, Acc) -> unescape(Rest, <<Acc/binary, "~">>);
unescape(<<"~1", Rest/binary>>, Acc) -> unescape(Rest, <<Acc/binary, "/">>);
unescape(<<"~", _/binary>>, _Acc) -> throw(invalid_args);
unescape(<<C, Rest/binary>>, Acc) -> unescape(Rest, <<Acc/binary, C>>);
unescape(<<>>, Acc) -> Acc.

%% The referenced input value in the containing object: {ok, Value} | missing
reference_value(Segments, Opts) ->
    walk(Segments, maps:get(?CONTAINER_OPT, Opts, #{})).

walk([], Value) ->
    {ok, Value};
walk([Segment|Segments], Value) ->
    case is_object(Value) of
        true ->
            case object_get(Segment, Value) of
                {ok, Value2} -> walk(Segments, Value2);
                error -> missing
            end;
        false when is_list(Value) ->
            case list_index(Segment) of
                Index when is_integer(Index), Index < length(Value) ->
                    walk(Segments, lists:nth(Index + 1, Value));
                _ ->
                    missing
            end;
        false ->
            missing
    end.

%% 0 or [1-9][0-9]*
list_index(<<"0">>) ->
    0;
list_index(<<C, _/binary>> = Segment) when C >= $1, C =< $9 ->
    case all_digits(Segment) of
        true -> binary_to_integer(Segment);
        false -> error
    end;
list_index(_Segment) ->
    error.

%% A JSON object: a map or a proplist ([{}] is an empty one).
is_object(Value) when is_map(Value) -> true;
is_object([{}]) -> true;
is_object([{_, _}|_]) -> true;
is_object(_) -> false.

object_to_list(Map) when is_map(Map) -> maps:to_list(Map);
object_to_list([{}]) -> [];
object_to_list(List) -> List.

%% Key-value pairs as an object of the same kind as Object.
object_like(Pairs, Object) when is_map(Object) -> maps:from_list(Pairs);
object_like([], _Object) -> [{}];
object_like(Pairs, _Object) -> Pairs.

%% A key of a decoded JSON object (a binary), or an atom key.
object_get(Key, Object) ->
    case object_find(Key, Object) of
        error when is_binary(Key) ->
            try binary_to_existing_atom(Key, utf8) of
                Atom -> object_find(Atom, Object)
            catch
                error:badarg -> error
            end;
        Result ->
            Result
    end.

object_find(Key, Map) when is_map(Map) ->
    maps:find(Key, Map);
object_find(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> {ok, Value};
        false -> error
    end.

key_string(Key) when is_binary(Key) -> Key;
key_string(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
key_string(Key) -> string_form(Key).

unique_item(Item, _Keys) when ?IS_EMPTY(Item) ->
    skip;
unique_item(Item, []) when ?IS_PRIMITIVE(Item) ->
    string_form(Item);
unique_item(Item, [_|_] = Keys) ->
    case is_object(Item) of
        true ->
            KeyValues = [case object_get(Key, Item) of
                             {ok, V} when ?IS_EMPTY(V) -> skip;
                             {ok, V} when ?IS_PRIMITIVE(V) -> string_form(V);
                             {ok, _V} -> throw(format_error);
                             error -> skip
                         end || Key <- Keys],
            case lists:member(skip, KeyValues) of
                true -> skip;
                false -> KeyValues
            end;
        false ->
            throw(format_error)
    end;
unique_item(_Item, _Keys) ->
    throw(format_error).

%% arguments
args_list([]) ->
    [];
args_list(Args) ->
    case is_object(Args) of
        true -> [Args];
        false when is_list(Args) -> Args;
        false -> [Args]
    end.

non_empty([]) -> throw(invalid_args);
non_empty(List) -> List.

primitive_arg(Arg) when ?IS_PRIMITIVE(Arg) -> string_form(Arg);
primitive_arg(_Arg) -> throw(invalid_args).

%% Numbers and strings are interchangeable in arguments (2.6).
string_arg(Arg) when is_binary(Arg); is_number(Arg) -> string_form(Arg);
string_arg(_Arg) -> throw(invalid_args).

non_empty_string_arg(Arg) ->
    case string_arg(Arg) of
        <<>> -> throw(invalid_args);
        String -> String
    end.

%% A key name, or an atom key of a schema with atom keys.
key_arg(Arg) when is_atom(Arg), Arg =/= null, Arg =/= undefined, not is_boolean(Arg) -> Arg;
key_arg(Arg) -> non_empty_string_arg(Arg).

%% A non-negative integer: 2, 2.0 or <<"2">>.
count_arg(Arg) when is_integer(Arg), Arg >= 0 ->
    Arg;
count_arg(Arg) when is_float(Arg), Arg >= 0, Arg == trunc(Arg) ->
    trunc(Arg);
count_arg(Arg) when is_binary(Arg) ->
    case decimal_string_to_number(Arg) of
        {ok, Number} -> count_arg(Number);
        error -> throw(invalid_args)
    end;
count_arg(_Arg) ->
    throw(invalid_args).

%% A primitive value other than null and "", or a non-empty list of them.
allowed_values(Value) when ?IS_PRIMITIVE(Value), Value =/= <<>> ->
    [string_form(Value)];
allowed_values([_|_] = Values) ->
    case is_object(Values) of
        true -> throw(invalid_args);
        false -> [case V of
                      _ when ?IS_PRIMITIVE(V), V =/= <<>> -> string_form(V);
                      _ -> throw(invalid_args)
                  end || V <- Values]
    end;
allowed_values(_Value) ->
    throw(invalid_args).

cases(Cases) ->
    case is_object(Cases) of
        true -> non_empty([{key_string(K), rules(Rules)} || {K, Rules} <- object_to_list(Cases)]);
        false -> throw(invalid_args)
    end.

%% Rules of a new rule: [] and null are schema errors, "any" means no
%% rules (2.6).
rules(null) ->
    throw(invalid_args);
rules(Rules) ->
    case is_empty_rules(Rules) of
        true -> throw(invalid_args);
        false -> Rules
    end.

is_empty_rules([]) -> true;
is_empty_rules([{}]) -> true;
is_empty_rules(Map) when is_map(Map) -> map_size(Map) =:= 0;
is_empty_rules([_|_] = List) -> lists:all(fun is_empty_rules/1, List);
is_empty_rules(_Rule) -> false.

uuid_version(<<"v", C>>) when C >= $1, C =< $8 -> C;
uuid_version(_Version) -> throw(invalid_args).

digest_length(<<"md5">>) -> 32;
digest_length(<<"sha1">>) -> 40;
digest_length(<<"sha256">>) -> 64;
digest_length(<<"sha384">>) -> 96;
digest_length(<<"sha512">>) -> 128;
digest_length(_Algorithm) -> throw(invalid_args).

%% formats
all_digits(<<>>) -> false;
all_digits(Bin) -> all_chars(Bin, fun(C) -> ?IS_DIGIT(C) end).

is_hex(<<>>) -> false;
is_hex(Bin) -> all_chars(Bin, fun(C) -> ?IS_HEX(C) end).

all_chars(<<C, Rest/binary>>, IsChar) ->
    IsChar(C) andalso all_chars(Rest, IsChar);
all_chars(<<>>, _IsChar) ->
    true.

is_uuid(<<A:8/binary, "-", B:4/binary, "-", C:4/binary, "-", D:4/binary, "-", E:12/binary>>,
        Versions) ->
    Hex = <<A/binary, B/binary, C/binary, D/binary, E/binary>>,
    <<Version, _/binary>> = C,
    <<Variant, _/binary>> = D,
    IsVariant = lists:member(Variant, "89abAB"),
    is_hex(Hex) andalso case Versions of
        any ->
            (IsVariant andalso Version >= $1 andalso Version =< $8)
                orelse all_chars(Hex, fun(Ch) -> Ch =:= $0 end)
                orelse all_chars(Hex, fun(Ch) -> Ch =:= $f orelse Ch =:= $F end);
        _ ->
            IsVariant andalso lists:member(Version, Versions)
    end;
is_uuid(_Value, _Versions) ->
    false.

%% v4 | v6 | false
ip_version(Ip, Version) ->
    case Version =/= v6 andalso is_ipv4(Ip) of
        true -> v4;
        false ->
            case Version =/= v4 andalso is_ipv6(Ip) of
                true -> v6;
                false -> false
            end
    end.

is_ipv4(Ip) ->
    case binary:split(Ip, <<".">>, [global]) of
        [_, _, _, _] = Octets -> lists:all(fun is_octet/1, Octets);
        _ -> false
    end.

%% 0-255, no leading zeros
is_octet(<<"0">>) ->
    true;
is_octet(<<C, _/binary>> = Octet) when C >= $1, C =< $9, byte_size(Octet) =< 3 ->
    all_digits(Octet) andalso binary_to_integer(Octet) =< 255;
is_octet(_Octet) ->
    false.

%% RFC 4291, section 2.2: one "::" may replace one or more groups of zeros,
%% the last two groups may be an IPv4 address.
is_ipv6(Ip) ->
    case binary:split(Ip, <<"::">>) of
        [Groups] ->
            ipv6_groups(Groups, true) =:= 8;
        [Left, Right] ->
            L = ipv6_groups(Left, false),
            R = ipv6_groups(Right, true),
            binary:match(Right, <<"::">>) =:= nomatch
                andalso is_integer(L) andalso is_integer(R) andalso L + R =< 7
    end.

%% The number of 16-bit groups, or false.
ipv6_groups(<<>>, _Ipv4) ->
    0;
ipv6_groups(Bin, Ipv4) ->
    Groups = binary:split(Bin, <<":">>, [global]),
    {Init, [Last]} = lists:split(length(Groups) - 1, Groups),
    case lists:all(fun is_ipv6_group/1, Init) of
        true ->
            case is_ipv6_group(Last) of
                true -> length(Groups);
                false when Ipv4 -> is_ipv4(Last) andalso length(Groups) + 1;
                false -> false
            end;
        false ->
            false
    end.

is_ipv6_group(Group) ->
    byte_size(Group) =< 4 andalso is_hex(Group).

%% 0-Max, no leading zeros
is_prefix(<<"0">>, _Max) ->
    true;
is_prefix(<<C, _/binary>> = Prefix, Max) when C >= $1, C =< $9, byte_size(Prefix) =< 3 ->
    all_digits(Prefix) andalso binary_to_integer(Prefix) =< Max;
is_prefix(_Prefix, _Max) ->
    false.

%% RFC 1123; the last label is not all digits, so an IPv4 address is not
%% a hostname.
is_hostname(Hostname) when byte_size(Hostname) =< 253 ->
    Labels = binary:split(Hostname, <<".">>, [global]),
    lists:all(fun is_label/1, Labels) andalso not all_digits(lists:last(Labels));
is_hostname(_Hostname) ->
    false.

is_label(Label) when byte_size(Label) >= 1, byte_size(Label) =< 63 ->
    binary:first(Label) =/= $- andalso binary:last(Label) =/= $-
        andalso all_chars(Label, fun(C) -> ?IS_DIGIT(C) orelse ?IS_ALPHA(C) orelse C =:= $- end);
is_label(_Label) ->
    false.

%% RFC 4648: the standard alphabet with padding; the URL-safe one with
%% complete padding or none.
is_base64(String, Alphabet) ->
    Size = byte_size(String),
    case binary:match(String, <<"=">>) of
        nomatch when Alphabet =:= url ->
            Size rem 4 =/= 1 andalso is_base64_chars(String, url);
        _ ->
            Size rem 4 =:= 0 andalso is_base64_chars(strip_padding(String, 2), Alphabet)
    end.

strip_padding(String, N) when N > 0, byte_size(String) > 0 ->
    case binary:last(String) of
        $= -> strip_padding(binary:part(String, 0, byte_size(String) - 1), N - 1);
        _ -> String
    end;
strip_padding(String, _N) ->
    String.

is_base64_chars(String, standard) ->
    all_chars(String, fun(C) -> ?IS_ALPHA(C) orelse ?IS_DIGIT(C) orelse C =:= $+ orelse C =:= $/ end);
is_base64_chars(String, url) ->
    all_chars(String, fun(C) -> ?IS_ALPHA(C) orelse ?IS_DIGIT(C) orelse C =:= $- orelse C =:= $_ end).

%% checksums
is_luhn(Digits) ->
    all_digits(Digits) andalso luhn_sum(lists:reverse(binary_to_list(Digits)), false, 0) rem 10 =:= 0.

luhn_sum([C|Rest], Double, Sum) ->
    Digit = case Double of
        true when (C - $0) * 2 > 9 -> (C - $0) * 2 - 9;
        true -> (C - $0) * 2;
        false -> C - $0
    end,
    luhn_sum(Rest, not Double, Sum + Digit);
luhn_sum([], _Double, Sum) ->
    Sum.

%% ISO 13616: CC, check digits, 11-30 of A-Z0-9; the number mod 97 is 1.
is_iban(<<A, B, C, D, Rest/binary>> = Iban) when byte_size(Iban) >= 15, byte_size(Iban) =< 34,
        A >= $A, A =< $Z, B >= $A, B =< $Z, ?IS_DIGIT(C), ?IS_DIGIT(D) ->
    IsUpperOrDigit = fun(Ch) -> ?IS_DIGIT(Ch) orelse (Ch >= $A andalso Ch =< $Z) end,
    all_chars(Rest, IsUpperOrDigit)
        andalso iban_mod97(<<Rest/binary, A, B, C, D>>, 0) =:= 1;
is_iban(_Iban) ->
    false.

iban_mod97(<<C, Rest/binary>>, N) when ?IS_DIGIT(C) ->
    iban_mod97(Rest, (N * 10 + C - $0) rem 97);
iban_mod97(<<C, Rest/binary>>, N) ->
    iban_mod97(Rest, (N * 100 + C - $A + 10) rem 97);
iban_mod97(<<>>, N) ->
    N.

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
