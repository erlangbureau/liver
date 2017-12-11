-module(liver_internals_SUITE).

-compile(export_all).

all() ->
    [
        float_to_binary,
        split,
        split_global,
        trim_left,
        trim_right,
        trim,
        to_lower,
        to_upper
    ].

float_to_binary(_Config) ->
    <<SmallDenorm/float>> = <<0,0,0,0,0,0,0,1>>,
    <<BigDenorm/float>> = <<0,15,255,255,255,255,255,255>>,
    <<SmallNorm/float>> = <<0,16,0,0,0,0,0,0>>,
    <<LargeNorm/float>> = <<127,239,255,255,255,255,255,255>>,
    TestCases = [
        {0.0,                   <<"0.0">>},
        {1.0,                   <<"1.0">>},
        {-1.0,                  <<"-1.0">>},
        {0.01,                  <<"0.01">>},
        {0.001,                 <<"0.001">>},
        {0.000001,              <<"0.000001">>},
        {0.0000001,             <<"1.0e-7">>},
        {100000.0,              <<"100000.0">>},
        {1000000.0,             <<"1.0e+6">>},
        {0.5,                   <<"0.5">>},
        {4503599627370496.0,    <<"4503599627370496.0">>},
        {math:pow(2, -1074),    <<"5.0e-324">>},
        %% small denormalized number
        %% 4.94065645841246544177e-324 =:= 5.0e-324
        {SmallDenorm,           <<"5.0e-324">>},
        {SmallDenorm,           SmallDenorm},
        %% large denormalized number
        %% 2.22507385850720088902e-308
        {BigDenorm,             <<"2.225073858507201e-308">>},
        {BigDenorm,             BigDenorm},
        %% small normalized number
        %% 2.22507385850720138309e-308
        {SmallNorm,             <<"2.2250738585072014e-308">>},
        {SmallNorm,             SmallNorm},
        %% large normalized number
        %% 1.79769313486231570815e+308
        {LargeNorm,             <<"1.7976931348623157e+308">>},
        {LargeNorm,             LargeNorm}
    ],
    float_to_binary_tests(TestCases).

split(_Config) ->
    TestCases = [
        {<<"Before After">>,        {<<"Before">>, <<"After">>}},
        {<<"До Після"/utf8>>,       {<<"До"/utf8>>, <<"Після"/utf8>>}},
        {<<" Після"/utf8>>,         {<<>>, <<"Після"/utf8>>}},
        {<<"До "/utf8>>,            {<<"До"/utf8>>, <<>>}},
        {<<"БезСпівпадінь"/utf8>>,  {<<"БезСпівпадінь"/utf8>>, <<>>}}
    ],
    Fun = fun(Text) -> liver_bstring:split(Text, <<" ">>) end,
    run_tests(TestCases, Fun).

split_global(_Config) ->
    TestCases = [
        {<<"One,two,three">>,       [<<"One">>,<<"two">>,<<"three">>]},
        {<<"Один,Два,Три"/utf8>>,   [<<"Один"/utf8>>,<<"Два"/utf8>>,<<"Три"/utf8>>]},
        {<<",Один,Два,Три"/utf8>>,  [<<>>, <<"Один"/utf8>>,<<"Два"/utf8>>,<<"Три"/utf8>>]}
    ],
    Fun = fun(Text) -> liver_bstring:split_global(Text, <<",">>) end,
    run_tests(TestCases, Fun).

trim_left(_Config) ->
    TestCases = [
        {<<>>,                      <<>>},
        {<<"  test  "/utf8>>,       <<"test  "/utf8>>},
        {<<"  Тест  "/utf8>>,       <<"Тест  "/utf8>>},
        {<<"\n\t\r Тест  "/utf8>>,  <<"Тест  "/utf8>>}
    ],
    run_tests(TestCases, fun liver_bstring:trim_left/1).

trim_right(_Config) ->
    TestCases = [
        {<<>>,                      <<>>},
        {<<"  test  "/utf8>>,       <<"  test"/utf8>>},
        {<<"  Тест  "/utf8>>,       <<"  Тест"/utf8>>},
        {<<"  Тест \n\t\r"/utf8>>,  <<"  Тест"/utf8>>}
    ],
    run_tests(TestCases, fun liver_bstring:trim_right/1).

trim(_Config) ->
    TestCases = [
        {<<"  test  "/utf8>>,       <<"test"/utf8>>},
        {<<"  Тест  "/utf8>>,       <<"Тест"/utf8>>}
    ],
    run_tests(TestCases, fun liver_bstring:trim/1).

to_lower(_Config) ->
    TestCases = [
        {<<"  TEST  "/utf8>>,       <<"  test  "/utf8>>},
        {<<"  ТЕСТ  "/utf8>>,       <<"  тест  "/utf8>>}
    ],
    run_tests(TestCases, fun liver_bstring:to_lower/1).

to_upper(_Config) ->
    TestCases = [
        {<<"  test  "/utf8>>,       <<"  TEST  ">>},
        {<<"  тест  "/utf8>>,       <<"  ТЕСТ  "/utf8>>}
    ],
    run_tests(TestCases, fun liver_bstring:to_upper/1).


%% internal
float_to_binary_tests([{Float, Binary}|TestCases]) when is_binary(Binary) ->
    case liver_float:to_binary(Float) =:= Binary of
        true -> float_to_binary_tests(TestCases);
        false ->
            ct:log("liver_float:to_binary(~p) =:= ~p~n", [Float, Binary]),
            {fail, unsuccessful}
    end;
float_to_binary_tests([{Float, Float2}|TestCases]) when is_float(Float2) ->
    case binary_to_float(liver_float:to_binary(Float)) =:= Float2 of
        true -> float_to_binary_tests(TestCases);
        false ->
            ct:log("binary_to_float(liver_float:to_binary(~p)) =:= ~p~n", [Float, Float2]),
            {fail, unsuccessful}
    end;
float_to_binary_tests([]) ->
    ok.

run_tests([{From, To}|TestCases], Fun) ->
    case Fun(From) =:= To of
        true -> run_tests(TestCases, Fun);
        false ->
            {fail, unsuccessful}
    end;
run_tests([], _Fun) ->
    ok.
