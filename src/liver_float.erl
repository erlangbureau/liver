%% @copyright 2007 Mochi Media, Inc.
%% @author Bob Ippolito <bob@mochimedia.com>
%% @reworked_by Serhii Kostiushkin <s.kostyushkin@gmail.com>
%% see original version at https://github.com/basho/mochiweb/blob/master/src/mochinum.erl

%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @doc Useful numeric algorithms for floats that cover some deficiencies
%% in the math module. More interesting is digits/1, which implements
%% the algorithm from:
%% http://www.cs.indiana.edu/~burger/fp/index.html
%% See also "Printing Floating-Point Numbers Quickly and Accurately"
%% in Proceedings of the SIGPLAN '96 Conference on Programming Language
%% Design and Implementation.

-module(liver_float).

%% API
-export([to_binary/1]).

%% IEEE 754 Float exponent bias
-define(FLOAT_BIAS, 1022).
-define(MIN_EXP, -1074).
-define(BIG_POW, 4503599627370496).

-define(ZEROS, <<"0000000000000000">>).

%% API
%% TODO
%% See http://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf
%% See https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf
to_binary(0.0) ->
    <<"0.0">>;
to_binary(Float) when is_float(Float) ->
    BinFloat = <<Float:64/float>>,                  %% to binary (double precision)
    <<Sign:1, Exp:11, Frac:52>> = BinFloat,         %% unpack
    Est = int_ceil(math:log10(abs(Float)) - 1.0e-10),
    {PointPlace, Digits} = to_printable_digits(Exp, Frac, Est),
    Digits2 = apply_decimal_part(PointPlace, Digits),
    add_sign(Sign, Digits2).

%% internal
int_ceil(X) ->
    T = trunc(X),
    case (X - T) of
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

to_printable_digits(Exp, Frac, Est) ->
    {Exp2, Frac2} = if
        Exp =:= 0 ->
            {?MIN_EXP, Frac};
        true ->
            {Exp - 53 - ?FLOAT_BIAS, Frac + (1 bsl 52)}
    end,
    Round = ((Frac2 band 1) =:= 0),
    case Exp2 >= 0 of
        true ->
            BExp2 = 1 bsl Exp2,
            case (Frac2 =/= ?BIG_POW) of
                true ->
                    scale((Frac2 * BExp2 * 2), 2, BExp2, BExp2, Round, Round, Est);
                false ->
                    scale((Frac2 * BExp2 * 4), 4, (BExp2 * 2), BExp2, Round, Round, Est)
            end;
        false ->
            case (Exp2 =:= ?MIN_EXP) orelse (Frac2 =/= ?BIG_POW) of
                true ->
                    scale((Frac2 * 2), 1 bsl (1 - Exp2), 1, 1, Round, Round, Est);
                false ->
                    scale((Frac2 * 4), 1 bsl (2 - Exp2), 2, 1, Round, Round, Est)
            end
    end.

scale(R, S, MPlus, MMinus, LowOk, HighOk, Est) ->
    case Est >= 0 of
        true ->
            Scale = int_pow(10, Est),
            fixup(R, S * Scale, MPlus, MMinus, Est, LowOk, HighOk);
        false ->
            Scale = int_pow(10, -Est),
            fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est, LowOk, HighOk)
    end.

fixup(R, S, MPlus, MMinus, K, LowOk, HighOk) ->
    TooLow = case HighOk of
        true    -> (R + MPlus) >= S;
        false   -> (R + MPlus) > S
    end,
    {K2, Digits} = case TooLow of
        true ->
            {K + 1, (generate(R, S, MPlus, MMinus, LowOk, HighOk))};
        false ->
            {K, (generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk))}
    end,
    {K2, trim_leading_zeros(Digits)}.

trim_leading_zeros(<<"0", Rest/binary>>) ->
    trim_leading_zeros(Rest);
trim_leading_zeros(Rest) ->
    Rest.

generate(R0, S, MPlus, MMinus, LowOk, HighOk) ->
    Digit = R0 div S,
    Rest = R0 rem S,
    TC1 = case LowOk of
        true    -> Rest =< MMinus;
        false   -> Rest < MMinus
    end,
    TC2 = case HighOk of
        true    -> (Rest + MPlus) >= S;
        false   -> (Rest + MPlus) > S
    end,
    case TC1 of
        false ->
            case TC2 of
                false ->
                    <<(to_char(Digit)), (generate(Rest * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk))/binary>>;
                true ->
                    <<(to_char(Digit+1))>>
            end;
        true ->
            case TC2 of
                false ->
                    <<(to_char(Digit))>>;
                true ->
                    case Rest * 2 < S of
                        true    -> <<(to_char(Digit))>>;
                        false   -> <<(to_char(Digit+1))>>
                    end
            end
    end.

to_char(Digit) -> $0 + Digit.

apply_decimal_part(0, S) ->
    <<"0.", S/binary>>;
apply_decimal_part(PointPlace, S) when PointPlace > 0 ->
    L = byte_size(S),
    case PointPlace - L of
        0 ->
            <<S/binary, ".0">>;
        N when N < 0 ->
            Size = L + N,
            <<S0:Size/binary, S1/binary>> = S,
            <<S0/binary, ".", S1/binary>>;
        N when N < 6 ->
            <<S/binary, ?ZEROS:N/binary, ".0">>;
        _ ->
            apply_exp(PointPlace, S)
    end;
apply_decimal_part(PointPlace, S) when PointPlace > -6 ->
    N = abs(PointPlace),
    <<"0.", ?ZEROS:N/binary, S/binary>>;
apply_decimal_part(PointPlace, S) ->
    apply_exp(PointPlace, S).

apply_exp(PointPlace, S) ->
    <<C, S0/binary>> = S,
    S1 = case S0 of
        <<>>  -> <<"0">>;
        _   -> S0
    end,
    Exp = case PointPlace < 0 of
        true    -> <<"e-">>;
        false   -> <<"e+">>
    end,
    ExpIndex = integer_to_binary(abs(PointPlace - 1)),
    <<C, ".", S1/binary, Exp/binary, ExpIndex/binary>>.

add_sign(0, Result) ->
    Result;
add_sign(1, Result) ->
    <<"-", Result/binary>>.

int_pow(10, 0) ->
    1;
int_pow(10, N) ->
    int_pow(10, abs(N), 1).

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    R2 = case N band 1 of
        1 -> R * X;
        0 -> R
    end,
    int_pow(X * X, N bsr 1, R2).
