-module(oliver_bstring).

%% API
-export([split/2]).
-export([split_global/2]).
-export([remove_chars/2]).
-export([leave_chars/2]).
-export([trim/1]).
-export([trim_left/1]).
-export([trim_right/1]).
-export([to_lower/1]).
-export([to_upper/1]).
-export([char_to_lower/1]).
-export([char_to_upper/1]).

-define(IS_BLANK(Blank),
    Blank == $\s;
    Blank == $\t;
    Blank == $\n;
    Blank == $\r
).

-type binary_string() :: unicode:unicode_binary().


%% API
-spec split(BinString, Pattern) -> {PartBeforePattern, PartAfterPattern}
when
    BinString           :: binary_string(),
    Pattern             :: binary_string(),
    PartBeforePattern   :: binary_string(),
    PartAfterPattern    :: binary_string().
split(BinString, Pattern) ->
    case binary:match(BinString, Pattern) of
        {A,B} ->
            <<Before:A/binary, _:B/binary, After/binary>> = BinString,
            {Before, After};
        nomatch ->
            {BinString, <<>>}
    end.

-spec split_global(BinString, Pattern) -> [BinStringPart]
when
    BinString       :: binary_string(),
    Pattern         :: binary_string(),
    BinStringPart   :: binary_string().
split_global(BinString, Pattern) ->
    split_global(BinString, Pattern, []).

split_global(BinString, Pattern, Acc) ->
    case binary:match(BinString, Pattern) of
        {0, B} ->
            <<_:B/binary, After/binary>> = BinString,
            split_global(After, Pattern, Acc);
        {A,B} ->
            <<Before:A/binary, _:B/binary, After/binary>> = BinString,
            split_global(After, Pattern, [Before|Acc]);
        nomatch ->
            lists:reverse([BinString|Acc])
    end.

remove_chars(BinString, Pattern) ->
    Pattern2 = unicode:characters_to_list(Pattern),
    Pattern3 = [unicode:characters_to_binary([P]) || P <- Pattern2],
    remove_chars(BinString, Pattern3, <<>>).

remove_chars(BinString, Pattern, Acc) ->
    case binary:match(BinString, Pattern) of
        {0, B} ->
            <<_:B/binary, After/binary>> = BinString,
            remove_chars(After, Pattern, Acc);
        {A, B} ->
            <<Before:A/binary, _:B/binary, After/binary>> = BinString,
            remove_chars(After, Pattern, <<Acc/binary, Before/binary>>);
        nomatch ->
            <<Acc/binary, BinString/binary>>
    end.

leave_chars(BinString, Pattern) ->
    Pattern2 = unicode:characters_to_list(Pattern),
    Pattern3 = [unicode:characters_to_binary([P]) || P <- Pattern2],
    leave_chars(BinString, Pattern3, <<>>).

leave_chars(BinString, Pattern, Acc) ->
    case binary:match(BinString, Pattern) of
        {A, B} ->
            <<_:A/binary, Match:B/binary, After/binary>> = BinString,
            leave_chars(After, Pattern, <<Acc/binary, Match/binary>>);
        nomatch ->
            <<Acc/binary>>
    end.

-spec trim(BinString1) -> BinString2
when
    BinString1 :: binary_string(),
    BinString2 :: binary_string().
trim(Binary) ->
    trim_right(trim_left(Binary)).

-spec trim_left(BinString1) -> BinString2
when
    BinString1 :: binary_string(),
    BinString2 :: binary_string().
trim_left(<<$\s, BinString/binary>>) ->
    trim_left(BinString);
trim_left(<<$\t, BinString/binary>>) ->
    trim_left(BinString);
trim_left(<<$\n, BinString/binary>>) ->
    trim_left(BinString);
trim_left(<<$\r, BinString/binary>>) ->
    trim_left(BinString);
trim_left(BinString) ->
    BinString.

-spec trim_right(BinString1) -> BinString2
when
    BinString1 :: binary_string(),
    BinString2 :: binary_string().
trim_right(<<>>) ->
    <<>>;
trim_right(BinString) ->
    case binary:last(BinString) of
        Blank when ?IS_BLANK(Blank) ->
            Size = size(BinString) - 1,
            <<Part:Size/binary, _/binary>> = BinString,
            trim_right(Part);
        _ ->
            BinString
    end.



-spec to_lower(BinString1) -> BinString2
when
    BinString1 :: binary_string(),
    BinString2 :: binary_string().
to_lower(BinString) ->
    << <<(char_to_lower(C))>> || <<C>> <= BinString>>.

-spec to_upper(BinString1) -> BinString2
when
    BinString1 :: binary_string(),
    BinString2 :: binary_string().
to_upper(BinString) ->
    << <<(char_to_upper(C))>> || <<C>> <= BinString>>.

%% TODO Add unicode support
-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(C)  -> C.

-spec char_to_upper(char()) -> char().
char_to_upper($a) -> $A;
char_to_upper($b) -> $B;
char_to_upper($c) -> $C;
char_to_upper($d) -> $D;
char_to_upper($e) -> $E;
char_to_upper($f) -> $F;
char_to_upper($g) -> $G;
char_to_upper($h) -> $H;
char_to_upper($i) -> $I;
char_to_upper($j) -> $J;
char_to_upper($k) -> $K;
char_to_upper($l) -> $L;
char_to_upper($m) -> $M;
char_to_upper($n) -> $N;
char_to_upper($o) -> $O;
char_to_upper($p) -> $P;
char_to_upper($q) -> $Q;
char_to_upper($r) -> $R;
char_to_upper($s) -> $S;
char_to_upper($t) -> $T;
char_to_upper($u) -> $U;
char_to_upper($v) -> $V;
char_to_upper($w) -> $W;
char_to_upper($x) -> $X;
char_to_upper($y) -> $Y;
char_to_upper($z) -> $Z;
char_to_upper(C)  -> C.
