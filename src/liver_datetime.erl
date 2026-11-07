-module(liver_datetime).

%% Strict ISO 8601 / RFC 3339 parsing for the LIVR date and time rules:
%% ASCII digits only, the whole value, no trailing newline.

%% API
-export([parse_date/1]).
-export([parse_datetime/1]).
-export([parse_time/1]).
-export([format_datetime/1]).
-export([today/0]).
-export([is_writable/1]).

-define(DAY_MS, 86400000).
-define(IS_DIGIT(C), (C >= $0 andalso C =< $9)).

%% API

%% YYYY-MM-DD -> {ok, {Y, M, D}} | error
parse_date(<<Y1, Y2, Y3, Y4, "-", M1, M2, "-", D1, D2>>)
        when ?IS_DIGIT(Y1), ?IS_DIGIT(Y2), ?IS_DIGIT(Y3), ?IS_DIGIT(Y4),
             ?IS_DIGIT(M1), ?IS_DIGIT(M2), ?IS_DIGIT(D1), ?IS_DIGIT(D2) ->
    Date = {((Y1 - $0) * 10 + Y2 - $0) * 100 + (Y3 - $0) * 10 + Y4 - $0,
            (M1 - $0) * 10 + M2 - $0,
            (D1 - $0) * 10 + D2 - $0},
    case calendar:valid_date(Date) of
        true -> {ok, Date};
        false -> error
    end;
parse_date(_) ->
    error.

%% YYYY-MM-DD "T" HH ":" MM [ ":" SS [ "." 1*9DIGIT ] ] ( "Z" / ( "+" / "-" ) HH ":" MM )
%% -> {ok, Milliseconds} in UTC since 0000-01-01T00:00:00Z (may be out of
%% the years 0000-9999, see is_writable/1), the fraction cut to milliseconds.
parse_datetime(<<Date:10/binary, "T", HH:2/binary, ":", MM:2/binary, Rest/binary>>) ->
    case {parse_date(Date), to_int(HH, 23), to_int(MM, 59), seconds(Rest)} of
        {{ok, D}, H, M, {S, Ms, Zone}} when is_integer(H), is_integer(M) ->
            case offset(Zone) of
                {ok, Offset} ->
                    Secs = calendar:date_to_gregorian_days(D) * 86400
                        + H * 3600 + M * 60 + S - Offset,
                    {ok, Secs * 1000 + Ms};
                error ->
                    error
            end;
        _ ->
            error
    end;
parse_datetime(_) ->
    error.

%% HH ":" MM [ ":" SS [ "." 1*9DIGIT ] ] -> {ok, Nanoseconds} since midnight
parse_time(<<HH:2/binary, ":", MM:2/binary, Rest/binary>>) ->
    case {to_int(HH, 23), to_int(MM, 59), time_seconds(Rest)} of
        {H, M, {ok, S, Ns}} when is_integer(H), is_integer(M) ->
            {ok, ((H * 60 + M) * 60 + S) * 1000000000 + Ns};
        _ ->
            error
    end;
parse_time(_) ->
    error.

%% Milliseconds from parse_datetime/1 -> <<"YYYY-MM-DDTHH:MM:SS.sssZ">>
format_datetime(Ms) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(Ms div 1000),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                                   [Y, Mo, D, H, Mi, S, Ms rem 1000])).

%% Within the years 0000-9999, so format_datetime/1 can write it.
is_writable(Ms) ->
    Ms >= 0 andalso Ms < calendar:date_to_gregorian_days({10000, 1, 1}) * ?DAY_MS.

%% The current date in UTC, in Gregorian days.
today() ->
    {Date, _Time} = calendar:system_time_to_universal_time(liver_clock:system_time(),
                                                           millisecond),
    calendar:date_to_gregorian_days(Date).

%% internal
seconds(<<":", SS:2/binary, Rest/binary>>) ->
    case to_int(SS, 59) of
        S when is_integer(S) ->
            case fraction(Rest) of
                {ok, Digits, Zone} ->
                    <<Ms:3/binary, _/binary>> = <<Digits/binary, "000">>,
                    {S, binary_to_integer(Ms), Zone};
                error ->
                    error
            end;
        error ->
            error
    end;
seconds(Zone) ->
    {0, 0, Zone}.

time_seconds(<<>>) ->
    {ok, 0, 0};
time_seconds(<<":", SS:2/binary, Rest/binary>>) ->
    case {to_int(SS, 59), fraction(Rest)} of
        {S, {ok, Digits, <<>>}} when is_integer(S) ->
            <<Ns:9/binary, _/binary>> = <<Digits/binary, "000000000">>,
            {ok, S, binary_to_integer(Ns)};
        _ ->
            error
    end;
time_seconds(_) ->
    error.

%% "." 1*9DIGIT, optional: {ok, Digits, Rest} | error
fraction(<<".", Rest/binary>>) ->
    case digits(Rest, 0) of
        N when N >= 1, N =< 9 ->
            <<Digits:N/binary, Rest2/binary>> = Rest,
            {ok, Digits, Rest2};
        _ ->
            error
    end;
fraction(Rest) ->
    {ok, <<>>, Rest}.

offset(<<"Z">>) ->
    {ok, 0};
offset(<<Sign, HH:2/binary, ":", MM:2/binary>>) when Sign =:= $+; Sign =:= $- ->
    case {to_int(HH, 23), to_int(MM, 59)} of
        {H, M} when is_integer(H), is_integer(M), Sign =:= $+ -> {ok, H * 3600 + M * 60};
        {H, M} when is_integer(H), is_integer(M) -> {ok, -(H * 3600 + M * 60)};
        _ -> error
    end;
offset(_) ->
    error.

digits(<<C, Rest/binary>>, N) when C >= $0, C =< $9 ->
    digits(Rest, N + 1);
digits(_, N) ->
    N.

to_int(Bin, Max) ->
    case to_int(Bin) of
        N when is_integer(N), N =< Max -> N;
        _ -> error
    end.

%% ASCII digits only: binary_to_integer/1 also takes a sign.
to_int(Bin) ->
    case digits(Bin, 0) =:= byte_size(Bin) of
        true -> binary_to_integer(Bin);
        false -> error
    end.
