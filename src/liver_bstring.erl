-module(liver_bstring).

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

-include("unicode.hrl").

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
        {A, B} ->
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
    << <<(maps:get(C, ?TO_LOWER, C))/utf8>> || <<C/utf8>> <= BinString>>.

-spec to_upper(BinString1) -> BinString2
when
    BinString1 :: binary_string(),
    BinString2 :: binary_string().
to_upper(BinString) ->
    << <<(maps:get(C, ?TO_UPPER, C))/utf8>> || <<C/utf8>> <= BinString>>.
