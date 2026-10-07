-module(erlang_standard_SUITE).

%% Human-readable asserts for the default erlang_standard rule set.
%% Each ok/3 or err/3 checks both map and proplist data forms.

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        presence,
        types,
        constraints,
        converters,
        special,
        nested,
        rule_sets,
        runtime_types,
        api_mutations
    ].

init_per_suite(Config) ->
    _ = application:load(liver),
    Config.

end_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Presence / nullability
%%--------------------------------------------------------------------

presence(_Config) ->
    %% required
    ok(#{a => required}, #{a => 1}, #{a => 1}),
    %% default_missing
    ok(#{a => {default,[7]}}, #{}, #{a => 7}),
    %% default_bare
    ok(#{a => {default,7}}, #{}, #{a => 7}),
    %% default_present
    ok(#{a => {default,[7]}}, #{a => 1}, #{a => 1}),
    %% is_null
    ok(#{a => is_null}, #{a => null}, #{a => null}),
    %% is_not_null
    ok(#{a => is_not_null}, #{a => 1}, #{a => 1}),
    %% is_undefined
    ok(#{a => is_undefined}, #{a => undefined}, #{a => undefined}),
    %% is_not_undefined
    ok(#{a => is_not_undefined}, #{a => 1}, #{a => 1}),
    %% required
    err(#{a => required}, #{}, #{a => required}),
    %% is_null
    err(#{a => is_null}, #{a => 1}, #{a => not_null}),
    %% is_not_null
    err(#{a => is_not_null}, #{a => null}, #{a => cannot_be_null}),
    %% is_undefined
    err(#{a => is_undefined}, #{a => 1}, #{a => not_undefined}),
    %% is_not_undefined
    err(#{a => is_not_undefined}, #{a => undefined}, #{a => cannot_be_undefined}),
    ok.

%%--------------------------------------------------------------------
%% Type predicates
%%--------------------------------------------------------------------

types(_Config) ->
    %% is_integer
    ok(#{n => is_integer}, #{n => 10}, #{n => 10}),
    %% is_string
    ok(#{s => is_string}, #{s => "hello"}, #{s => "hello"}),
    %% is_utf8_binary
    ok(#{b => is_utf8_binary}, #{b => <<"hello">>}, #{b => <<"hello">>}),
    %% is_atom
    ok(#{a => is_atom}, #{a => foo}, #{a => foo}),
    %% is_float
    ok(#{x => is_float}, #{x => 1.5}, #{x => 1.5}),
    %% is_number
    ok(#{x => is_number}, #{x => 3}, #{x => 3}),
    %% is_non_neg_integer
    ok(#{n => is_non_neg_integer}, #{n => 0}, #{n => 0}),
    %% is_pos_integer
    ok(#{n => is_pos_integer}, #{n => 1}, #{n => 1}),
    %% is_tuple
    ok(#{t => {is_tuple,[{size,2}]}}, #{t => {1,2}}, #{t => {1,2}}),
    %% is_integer_positive
    ok(#{n => {is_integer,[positive]}}, #{n => 1}, #{n => 1}),
    %% is_integer_negative
    ok(#{n => {is_integer,[negative]}}, #{n => -1}, #{n => -1}),
    %% is_integer_non_neg_arg
    ok(#{n => {is_integer,[non_neg]}}, #{n => 0}, #{n => 0}),
    %% is_boolean
    ok(#{b => is_boolean}, #{b => true}, #{b => true}),
    %% is_list
    ok(#{l => is_list}, #{l => [1]}, #{l => [1]}),
    %% is_list_empty
    ok(#{l => {is_list,[empty]}}, #{l => []}, #{l => []}),
    %% is_list_not_empty
    ok(#{l => {is_list,[not_empty]}}, #{l => [1]}, #{l => [1]}),
    %% is_string_empty
    ok(#{s => {is_string,[empty]}}, #{s => []}, #{s => []}),
    %% is_string_not_empty
    ok(#{s => {is_string,[not_empty]}}, #{s => "a"}, #{s => "a"}),
    %% is_utf8_binary_empty
    ok(#{b => {is_utf8_binary,[empty]}}, #{b => <<>>}, #{b => <<>>}),
    %% is_utf8_binary_not_empty
    ok(#{b => {is_utf8_binary,[not_empty]}}, #{b => <<"a">>}, #{b => <<"a">>}),
    %% is_binary
    ok(#{b => is_binary}, #{b => <<0,255>>}, #{b => <<0,255>>}),
    %% is_binary_empty
    ok(#{b => {is_binary,[empty]}}, #{b => <<>>}, #{b => <<>>}),
    %% is_binary_not_empty
    ok(#{b => {is_binary,[not_empty]}}, #{b => <<"x">>}, #{b => <<"x">>}),
    %% is_map
    ok(#{m => is_map}, #{m => #{a => 1}}, #{m => #{a => 1}}),
    %% is_proplist
    ok(#{p => is_proplist}, #{p => [{a,1}]}, #{p => [{a,1}]}),
    %% is_tuple_plain
    ok(#{t => is_tuple}, #{t => {1}}, #{t => {1}}),
    %% is_term
    ok(#{t => is_term}, #{t => {x,y}}, #{t => {x,y}}),
    %% is_integer
    err(#{n => is_integer}, #{n => <<"10">>}, #{n => not_integer}),
    %% is_string
    err(#{s => is_string}, #{s => <<"hello">>}, #{s => not_string}),
    %% is_non_neg_integer
    err(#{n => is_non_neg_integer}, #{n => -1}, #{n => not_non_neg_integer}),
    %% is_integer_under_livr_spec
    err(#{n => is_integer}, #{n => 10}, #{n => {unimplemented_rule,is_integer}}, #{rule_set => livr_spec}),
    %% is_integer_positive
    err(#{n => {is_integer,[positive]}}, #{n => -1}, #{n => not_integer}),
    %% is_pos_integer
    err(#{n => is_pos_integer}, #{n => 0}, #{n => not_pos_integer}),
    %% is_float
    err(#{n => is_float}, #{n => 1}, #{n => not_float}),
    %% is_number
    err(#{n => is_number}, #{n => a}, #{n => not_number}),
    %% is_boolean
    err(#{b => is_boolean}, #{b => 1}, #{b => not_boolean}),
    %% is_atom
    err(#{a => is_atom}, #{a => <<"x">>}, #{a => not_atom}),
    %% is_list_not_empty
    err(#{l => {is_list,[not_empty]}}, #{l => []}, #{l => cannot_be_empty}),
    %% is_list_empty
    err(#{l => {is_list,[empty]}}, #{l => [1]}, #{l => not_empty}),
    %% is_list
    err(#{l => is_list}, #{l => <<>>}, #{l => not_list}),
    %% is_string_not_empty
    err(#{s => {is_string,[not_empty]}}, #{s => []}, #{s => cannot_be_empty}),
    %% is_string_empty
    err(#{s => {is_string,[empty]}}, #{s => "a"}, #{s => not_empty}),
    %% is_string_bad_chars
    err(#{s => is_string}, #{s => [1.5]}, #{s => not_string}),
    %% is_utf8_binary_not_empty
    err(#{b => {is_utf8_binary,[not_empty]}}, #{b => <<>>}, #{b => cannot_be_empty}),
    %% is_utf8_binary_empty
    err(#{b => {is_utf8_binary,[empty]}}, #{b => <<"a">>}, #{b => not_empty}),
    %% is_utf8_binary_invalid
    err(#{b => is_utf8_binary}, #{b => <<"ÿ">>}, #{b => not_utf8_binary}),
    %% is_utf8_binary
    err(#{b => is_utf8_binary}, #{b => 1}, #{b => not_utf8_binary}),
    %% is_binary_not_empty
    err(#{b => {is_binary,[not_empty]}}, #{b => <<>>}, #{b => cannot_be_empty}),
    %% is_binary_empty
    err(#{b => {is_binary,[empty]}}, #{b => <<"x">>}, #{b => not_empty}),
    %% is_binary
    err(#{b => is_binary}, #{b => []}, #{b => not_binary}),
    %% is_map
    err(#{m => is_map}, #{m => []}, #{m => not_map}),
    %% is_proplist
    err(#{p => is_proplist}, #{p => [1]}, #{p => not_proplist}),
    %% is_proplist_map
    err(#{p => is_proplist}, #{p => #{}}, #{p => not_proplist}),
    %% is_tuple_size
    err(#{t => {is_tuple,[{size,2}]}}, #{t => {1}}, #{t => wrong_tuple_size}),
    %% is_tuple
    err(#{t => is_tuple}, #{t => []}, #{t => not_tuple}),
    ok.

%%--------------------------------------------------------------------
%% Constraints
%%--------------------------------------------------------------------

constraints(_Config) ->
    %% one_of_terms
    ok(#{v => {one_of_terms,[[a,b]]}}, #{v => a}, #{v => a}),
    %% range
    ok(#{v => {range,[{1,10}]}}, #{v => 5}, #{v => 5}),
    %% byte_size
    ok(#{b => {byte_size,[{eq,2}]}}, #{b => <<"ab">>}, #{b => <<"ab">>}),
    %% length
    ok(#{l => {length,[{eq,2}]}}, #{l => [1,2]}, #{l => [1,2]}),
    %% member
    ok(#{v => {member,[[a,b]]}}, #{v => a}, #{v => a}),
    %% member_flat
    ok(#{v => {member,[a,b]}}, #{v => a}, #{v => a}),
    %% range_list_args
    ok(#{v => {range,[1,10]}}, #{v => 5}, #{v => 5}),
    %% byte_size_min
    ok(#{b => {byte_size,[{min,2}]}}, #{b => <<"abc">>}, #{b => <<"abc">>}),
    %% byte_size_max
    ok(#{b => {byte_size,[{max,2}]}}, #{b => <<"ab">>}, #{b => <<"ab">>}),
    %% byte_size_between
    ok(#{b => {byte_size,[{between,1,3}]}}, #{b => <<"ab">>}, #{b => <<"ab">>}),
    %% bit_size
    ok(#{b => {bit_size,[{eq,3}]}}, #{b => <<1:3>>}, #{b => <<1:3>>}),
    %% bit_size_min
    ok(#{b => {bit_size,[{min,2}]}}, #{b => <<1:3>>}, #{b => <<1:3>>}),
    %% bit_size_max
    ok(#{b => {bit_size,[{max,8}]}}, #{b => <<1:3>>}, #{b => <<1:3>>}),
    %% tuple_size
    ok(#{t => {tuple_size,[{eq,2}]}}, #{t => {a,b}}, #{t => {a,b}}),
    %% map_size
    ok(#{m => {map_size,[{eq,2}]}}, #{m => #{a => 1,b => 2}}, #{m => #{a => 1, b => 2}}),
    %% map_size_min
    ok(#{m => {map_size,[{min,1}]}}, #{m => #{a => 1}}, #{m => #{a => 1}}),
    %% map_size_max
    ok(#{m => {map_size,[{max,0}]}}, #{m => #{}}, #{m => #{}}),
    %% length_min
    ok(#{l => {length,[{min,2}]}}, #{l => [1,2,3]}, #{l => [1,2,3]}),
    %% length_max
    ok(#{l => {length,[{max,2}]}}, #{l => [1]}, #{l => [1]}),
    %% length_between
    ok(#{l => {length,[{between,1,3}]}}, #{l => [1,2]}, #{l => [1,2]}),
    %% one_of_terms
    err(#{v => {one_of_terms,[[a,b]]}}, #{v => c}, #{v => not_allowed_value}),
    %% range
    err(#{v => {range,[{1,10}]}}, #{v => 11}, #{v => too_high}),
    %% one_of_terms_format
    err(#{v => {one_of_terms,not_a_list}}, #{v => a}, #{v => format_error}),
    %% member
    err(#{v => {member,[[a,b]]}}, #{v => c}, #{v => not_member}),
    %% member_format
    err(#{v => {member,not_a_list}}, #{v => a}, #{v => format_error}),
    %% range_too_low
    err(#{v => {range,[1,10]}}, #{v => 0}, #{v => too_low}),
    %% range_format
    err(#{v => {range,[only_one]}}, #{v => 1}, #{v => format_error}),
    %% range_not_number
    err(#{v => {range,[1,10]}}, #{v => x}, #{v => not_number}),
    %% byte_size
    err(#{b => {byte_size,[{eq,3}]}}, #{b => <<"ab">>}, #{b => wrong_byte_size}),
    %% byte_size_not_binary
    err(#{b => {byte_size,[{eq,1}]}}, #{b => []}, #{b => not_binary}),
    %% bit_size
    err(#{b => {bit_size,[{eq,8}]}}, #{b => <<1:3>>}, #{b => wrong_bit_size}),
    %% bit_size_not_binary
    err(#{b => {bit_size,[{eq,1}]}}, #{b => []}, #{b => not_binary}),
    %% tuple_size
    err(#{t => {tuple_size,[{eq,3}]}}, #{t => {a,b}}, #{t => wrong_tuple_size}),
    %% tuple_size_not_tuple
    err(#{t => {tuple_size,[{eq,1}]}}, #{t => []}, #{t => not_tuple}),
    %% map_size
    err(#{m => {map_size,[{eq,2}]}}, #{m => #{a => 1}}, #{m => wrong_map_size}),
    %% map_size_not_map
    err(#{m => {map_size,[{eq,1}]}}, #{m => []}, #{m => not_map}),
    %% length
    err(#{l => {length,[{eq,3}]}}, #{l => [1]}, #{l => wrong_length}),
    %% length_not_list
    err(#{l => {length,[{eq,1}]}}, #{l => <<>>}, #{l => not_list}),
    ok.

%%--------------------------------------------------------------------
%% Converters
%%--------------------------------------------------------------------

converters(_Config) ->
    %% to_integer
    ok(#{n => to_integer}, #{n => <<"10">>}, #{n => 10}),
    %% to_float
    ok(#{n => to_float}, #{n => <<"10">>}, #{n => 10.0}),
    %% to_utf8_binary
    ok(#{b => to_utf8_binary}, #{b => "hi"}, #{b => <<"hi">>}),
    %% to_existing_atom
    ok(#{a => to_existing_atom}, #{a => <<"true">>}, #{a => true}),
    %% to_map
    ok(#{m => to_map}, #{m => [{a,1}]}, #{m => #{a => 1}}),
    %% to_proplist
    ok(#{p => to_proplist}, #{p => #{a => 1}}, #{p => [{a,1}]}),
    %% to_integer_int
    ok(#{n => to_integer}, #{n => 10}, #{n => 10}),
    %% to_integer_list
    ok(#{n => to_integer}, #{n => "10"}, #{n => 10}),
    %% to_integer_float
    ok(#{n => to_integer}, #{n => 10.9}, #{n => 10}),
    %% to_float_float
    ok(#{n => to_float}, #{n => 1.5}, #{n => 1.5}),
    %% to_float_int
    ok(#{n => to_float}, #{n => 2}, #{n => 2.0}),
    %% to_float_bin_decimal
    ok(#{n => to_float}, #{n => <<"1.5">>}, #{n => 1.5}),
    %% to_float_bin_int
    ok(#{n => to_float}, #{n => <<"2">>}, #{n => 2.0}),
    %% to_float_list_decimal
    ok(#{n => to_float}, #{n => "1.5"}, #{n => 1.5}),
    %% to_float_list_int
    ok(#{n => to_float}, #{n => "2"}, #{n => 2.0}),
    %% to_boolean_true
    ok(#{b => to_boolean}, #{b => true}, #{b => true}),
    %% to_boolean_false
    ok(#{b => to_boolean}, #{b => false}, #{b => false}),
    %% to_boolean_0
    ok(#{b => to_boolean}, #{b => 0}, #{b => false}),
    %% to_boolean_1
    ok(#{b => to_boolean}, #{b => 1}, #{b => true}),
    %% to_boolean_empty_list
    ok(#{b => to_boolean}, #{b => []}, #{b => false}),
    %% to_boolean_str0
    ok(#{b => to_boolean}, #{b => "0"}, #{b => false}),
    %% to_boolean_str_false
    ok(#{b => to_boolean}, #{b => "false"}, #{b => false}),
    %% to_boolean_str_true
    ok(#{b => to_boolean}, #{b => "true"}, #{b => true}),
    %% to_boolean_empty_bin
    ok(#{b => to_boolean}, #{b => <<>>}, #{b => false}),
    %% to_boolean_bin0
    ok(#{b => to_boolean}, #{b => <<"0">>}, #{b => false}),
    %% to_boolean_bin_false
    ok(#{b => to_boolean}, #{b => <<"false">>}, #{b => false}),
    %% to_boolean_bin_true
    ok(#{b => to_boolean}, #{b => <<"true">>}, #{b => true}),
    %% to_boolean_undefined
    ok(#{b => to_boolean}, #{b => undefined}, #{b => false}),
    %% to_boolean_null
    ok(#{b => to_boolean}, #{b => null}, #{b => false}),
    %% to_boolean_other
    ok(#{b => to_boolean}, #{b => other}, #{b => true}),
    %% to_string_list
    ok(#{s => to_string}, #{s => "hi"}, #{s => "hi"}),
    %% to_string_bin
    ok(#{s => to_string}, #{s => <<"hi">>}, #{s => "hi"}),
    %% to_string_atom
    ok(#{s => to_string}, #{s => ok}, #{s => "ok"}),
    %% to_string_int
    ok(#{s => to_string}, #{s => 12}, #{s => "12"}),
    %% to_utf8_binary_atom
    ok(#{b => to_utf8_binary}, #{b => ok}, #{b => <<"ok">>}),
    %% to_utf8_binary_int
    ok(#{b => to_utf8_binary}, #{b => 12}, #{b => <<"12">>}),
    %% to_binary
    ok(#{b => to_binary}, #{b => <<"hi">>}, #{b => <<"hi">>}),
    %% to_binary_list
    ok(#{b => to_binary}, #{b => "hi"}, #{b => <<"hi">>}),
    %% to_binary_atom
    ok(#{b => to_binary}, #{b => ok}, #{b => <<"ok">>}),
    %% to_binary_int
    ok(#{b => to_binary}, #{b => 12}, #{b => <<"12">>}),
    %% to_atom
    ok(#{a => to_atom}, #{a => foo}, #{a => foo}),
    %% to_atom_bin
    ok(#{a => to_atom}, #{a => <<"foo">>}, #{a => foo}),
    %% to_atom_list
    ok(#{a => to_atom}, #{a => "foo"}, #{a => foo}),
    %% to_existing_atom_list
    ok(#{a => to_existing_atom}, #{a => "true"}, #{a => true}),
    %% to_existing_atom_atom
    ok(#{a => to_existing_atom}, #{a => true}, #{a => true}),
    %% to_list
    ok(#{l => to_list}, #{l => "hi"}, #{l => "hi"}),
    %% to_list_bin
    ok(#{l => to_list}, #{l => <<"hi">>}, #{l => "hi"}),
    %% to_list_atom
    ok(#{l => to_list}, #{l => ok}, #{l => "ok"}),
    %% to_list_tuple
    ok(#{l => to_list}, #{l => {1,2}}, #{l => [1,2]}),
    %% to_map_identity
    ok(#{m => to_map}, #{m => #{a => 1}}, #{m => #{a => 1}}),
    %% to_proplist_identity
    ok(#{p => to_proplist}, #{p => [{a,1}]}, #{p => [{a,1}]}),
    %% to_existing_atom
    err(#{a => to_existing_atom}, #{a => <<"no_such_atom_xyz_liver_test">>}, #{a => cant_be_atom}),
    %% to_integer
    err(#{n => to_integer}, #{n => <<"x">>}, #{n => cant_be_integer}),
    %% to_integer_list
    err(#{n => to_integer}, #{n => "x"}, #{n => cant_be_integer}),
    %% to_integer_bad
    err(#{n => to_integer}, #{n => []}, #{n => cant_be_integer}),
    %% to_float
    err(#{n => to_float}, #{n => <<"x">>}, #{n => cant_be_float}),
    %% to_float_list
    err(#{n => to_float}, #{n => "x"}, #{n => cant_be_float}),
    %% to_float_bad
    err(#{n => to_float}, #{n => []}, #{n => cant_be_float}),
    %% to_string
    err(#{s => to_string}, #{s => [1.5]}, #{s => cant_be_string}),
    %% to_string_bad_bin
    err(#{s => to_string}, #{s => <<"ÿ">>}, #{s => cant_be_string}),
    %% to_utf8_binary
    err(#{b => to_utf8_binary}, #{b => <<"ÿ">>}, #{b => cant_be_binary}),
    %% to_utf8_binary_bad_list
    err(#{b => to_utf8_binary}, #{b => [1114112]}, #{b => cant_be_binary}),
    %% to_binary
    err(#{b => to_binary}, #{b => 1.5}, #{b => cant_be_binary}),
    %% to_atom
    err(#{a => to_atom}, #{a => 1}, #{a => cant_be_atom}),
    %% to_existing_atom_list
    err(#{a => to_existing_atom}, #{a => "no_such_atom_xyz_liver_cov"}, #{a => cant_be_atom}),
    %% to_existing_atom_bad
    err(#{a => to_existing_atom}, #{a => 1}, #{a => cant_be_atom}),
    %% to_list
    err(#{l => to_list}, #{l => 1}, #{l => cant_be_list}),
    %% to_map
    err(#{m => to_map}, #{m => [1]}, #{m => cant_be_map}),
    %% to_map_bad
    err(#{m => to_map}, #{m => 1}, #{m => cant_be_map}),
    %% to_proplist
    err(#{p => to_proplist}, #{p => [1]}, #{p => cant_be_proplist}),
    %% to_proplist_bad
    err(#{p => to_proplist}, #{p => 1}, #{p => cant_be_proplist}),
    ok.

%%--------------------------------------------------------------------
%% email / url / iso_date (incl. internationalized addresses)
%%--------------------------------------------------------------------

special(_Config) ->
    %% ASCII
    ok(#{e => email}, #{e => <<"user@example.com">>}, #{e => <<"user@example.com">>}),
    ok(#{e => email}, #{e => "a@b.co"}, #{e => <<"a@b.co">>}),
    %% Ukrainian / Belarusian (Cyrillic IDN)
    ok(#{e => email},
       #{e => <<"квіточка@пошта.укр"/utf8>>},
       #{e => <<"квіточка@пошта.укр"/utf8>>}),
    ok(#{e => email},
       #{e => <<"карыстальнік@пошта.бел"/utf8>>},
       #{e => <<"карыстальнік@пошта.бел"/utf8>>}),
    %% CJK / Indic / Greek / Latin-1
    ok(#{e => email},
       #{e => <<"用户@例子.广告"/utf8>>},
       #{e => <<"用户@例子.广告"/utf8>>}),
    ok(#{e => email},
       #{e => <<"अजय@डाटा.भारत"/utf8>>},
       #{e => <<"अजय@डाटा.भारत"/utf8>>}),
    ok(#{e => email},
       #{e => <<"θσερ@εχαμπλε.ψομ"/utf8>>},
       #{e => <<"θσερ@εχαμπλε.ψομ"/utf8>>}),
    ok(#{e => email},
       #{e => <<"Dörte@Sörensen.example.com"/utf8>>},
       #{e => <<"Dörte@Sörensen.example.com"/utf8>>}),
    %% URL: ASCII + punycode + Unicode IDN hosts
    ok(#{u => url}, #{u => <<"https://example.com">>}, #{u => <<"https://example.com">>}),
    ok(#{u => url}, #{u => "http://example.com"}, #{u => <<"http://example.com">>}),
    ok(#{u => url},
       #{u => <<"https://xn--e1afmkfd.xn--p1ai">>},
       #{u => <<"https://xn--e1afmkfd.xn--p1ai">>}),
    ok(#{u => url},
       #{u => <<"https://пошта.укр"/utf8>>},
       #{u => <<"https://пошта.укр"/utf8>>}),
    ok(#{u => url},
       #{u => <<"https://прыклад.бел/path"/utf8>>},
       #{u => <<"https://прыклад.бел/path"/utf8>>}),
    ok(#{u => url},
       #{u => <<"http://例子.广告/"/utf8>>},
       #{u => <<"http://例子.广告/"/utf8>>}),
    ok(#{u => url},
       #{u => <<"https://डाटा.भारत"/utf8>>},
       #{u => <<"https://डाटा.भारत"/utf8>>}),
    %% iso_date
    ok(#{d => iso_date}, #{d => <<"2020-01-02">>}, #{d => {2020,1,2}}),
    ok(#{d => iso_date}, #{d => {2020,1,2}}, #{d => {2020,1,2}}),
    %% negatives
    err(#{e => email}, #{e => <<"not-an-email">>}, #{e => wrong_email}),
    err(#{e => email}, #{e => <<"a@@b.com">>}, #{e => wrong_email}),
    err(#{e => email}, #{e => 1}, #{e => format_error}),
    err(#{u => url}, #{u => <<"ftp://example.com">>}, #{u => wrong_url}),
    err(#{u => url}, #{u => <<"not-a-url">>}, #{u => wrong_url}),
    err(#{u => url}, #{u => 1}, #{u => format_error}),
    err(#{d => iso_date}, #{d => {2020,2,30}}, #{d => wrong_date}),
    err(#{d => iso_date}, #{d => <<"2020-13-01">>}, #{d => wrong_date}),
    err(#{d => iso_date}, #{d => <<"bad">>}, #{d => wrong_date}),
    err(#{d => iso_date}, #{d => 1}, #{d => format_error}),
    ok.

%%--------------------------------------------------------------------
%% Nested structures
%%--------------------------------------------------------------------

nested(_Config) ->
    %% nested_map
    ok(#{obj => {nested_map,#{name => [required,is_atom]}}}, #{obj => #{name => bob}}, #{obj => #{name => bob}}),
    %% nested_proplist
    ok(#{o => {nested_proplist,[{n,is_integer}]}}, #{o => [{n,1}]}, #{o => [{n,1}]}),
    %% nested_proplist_map_value
    ok(#{o => {nested_proplist,#{n => is_integer}}}, #{o => #{n => 1}}, #{o => #{n => 1}}),
    %% nested_list
    ok(#{l => {nested_list,is_integer}}, #{l => [1,2]}, #{l => [1,2]}),
    %% nested_map
    err(#{obj => {nested_map,#{name => [required,is_atom]}}}, #{obj => #{}}, #{obj => #{name => required}}),
    %% nested_map_format
    err(#{o => {nested_map,#{n => is_integer}}}, #{o => 1}, #{o => format_error}),
    %% nested_proplist
    err(#{o => {nested_proplist,#{n => is_integer}}}, #{o => 1}, #{o => not_proplist}),
    %% nested_list
    err(#{l => {nested_list,is_integer}}, #{l => <<>>}, #{l => not_list}),
    ok.

%%--------------------------------------------------------------------
%% rule_set / return / schema shapes
%%--------------------------------------------------------------------

rule_sets(_Config) ->
    %% rule_set_livr_spec
    ok(#{n => integer}, #{n => <<"10">>}, #{n => 10}, #{rule_set => livr_spec}),
    %% rule_set_compose
    ok(#{m => is_pos_integer,n => integer}, #{m => 3,n => <<"10">>}, #{m => 3, n => 10}, #{rule_set => [livr_spec,erlang_standard]}),
    %% rule_set_mixed_alias
    ok(#{n => is_integer}, #{n => 10}, #{n => 10}, #{rule_set => {mixed,erlang_standard}}),
    %% return_proplist
    ok(#{a => is_integer}, #{a => 1}, [{a,1}], #{return => proplist}),
    %% return_map
    ok(#{a => is_integer}, [{a,1}], #{a => 1}, #{return => map}),
    %% livr_compatible
    ok(#{n => integer}, #{n => <<"10">>}, #{n => 10}, #{livr_compatible => true}),
    %% rule_set_mixed_livr_first
    ok(#{n => integer}, #{n => <<"10">>}, #{n => 10}, #{rule_set => {mixed,livr_spec}}),
    %% rule_set_inline_map
    ok(#{n => is_integer}, #{n => 10}, #{n => 10}, #{rule_set => #{is_integer => liver_standard_rules}}),
    %% list_schema
    ok([is_integer], [1,2], [1,2]),
    %% term_schema
    ok(is_integer, 3, 3),
    %% binary_rule_name
    ok(#{<<"n">> => <<"is_integer">>}, #{<<"n">> => 1}, #{<<"n">> => 1}),
    %% binary_rule_tuple
    ok(#{<<"n">> => [{<<"is_integer">>,[]}]}, #{<<"n">> => 1}, #{<<"n">> => 1}),
    %% binary_rule_list
    ok(#{<<"n">> => [<<"is_integer">>]}, #{<<"n">> => 1}, #{<<"n">> => 1}),
    %% livr_name_unavailable
    err(#{n => integer}, #{n => <<"10">>}, #{n => {unimplemented_rule,integer}}),
    %% strict_unknown_field
    err(#{a => is_integer}, #{a => 1,x => 2}, #{x => unknown_field}, #{strict => true}),
    ok.

%%--------------------------------------------------------------------
%% Runtime-only values (pid/ref/port/fun) — both data forms
%%--------------------------------------------------------------------

runtime_types(_Config) ->
    Pid = self(),
    Ref = make_ref(),
    Port = open_port({spawn, "true"}, []),
    Fun0 = fun() -> ok end,
    Fun1 = fun(_) -> ok end,
    try
        ok(#{p => is_pid}, #{p => Pid}, #{p => Pid}),
        err(#{p => is_pid}, #{p => 1}, #{p => not_pid}),
        ok(#{r => is_ref}, #{r => Ref}, #{r => Ref}),
        err(#{r => is_ref}, #{r => 1}, #{r => not_ref}),
        ok(#{p => is_port}, #{p => Port}, #{p => Port}),
        err(#{p => is_port}, #{p => 1}, #{p => not_port}),
        ok(#{f => is_fun}, #{f => Fun0}, #{f => Fun0}),
        ok(#{f => {is_fun, [{arity, 1}]}}, #{f => Fun1}, #{f => Fun1}),
        err(#{f => {is_fun, [{arity, 2}]}}, #{f => Fun1}, #{f => not_fun}),
        err(#{f => is_fun}, #{f => 1}, #{f => not_fun}),
        err(#{s => to_string}, #{s => Pid}, #{s => cant_be_string}),
        err(#{b => to_utf8_binary}, #{b => Pid}, #{b => cant_be_binary}),
        err(#{b => to_utf8_binary}, #{b => [1.5]}, #{b => format_error}),
        err(#{b => to_binary}, #{b => [Pid]}, #{b => cant_be_binary})
    after
        catch erlang:port_close(Port)
    end,
    ok.

%%--------------------------------------------------------------------
%% API that mutates application env
%%--------------------------------------------------------------------

api_mutations(_Config) ->
    ?assertEqual(liver_standard_rules, liver:which(is_integer)),
    ok = liver:add_rule_set(cov_set, #{cov_ok => liver_standard_rules}),
    ?assertEqual({ok, #{n => 1}},
                 liver:validate(#{n => is_integer}, #{n => 1},
                                #{rule_set => [erlang_standard, cov_set]})),
    ?assertError({unknown_rule_set, no_such_set},
                 liver:validate(#{n => is_integer}, #{n => 1},
                                #{rule_set => no_such_set})),
    ?assertError(empty_rule_set,
                 liver:validate(#{n => is_integer}, #{n => 1},
                                #{rule_set => []})),
    ?assertError({invalid_rule_set, 123},
                 liver:validate(#{n => is_integer}, #{n => 1},
                                #{rule_set => 123})),
    ok = liver:custom_error(not_integer, <<"CUSTOM_NOT_INT">>),
    ?assertEqual({error, #{n => <<"CUSTOM_NOT_INT">>}},
                 liver:validate(#{n => is_integer}, #{n => <<"x">>})),
    ok = liver:custom_error(not_integer, <<"NOT_INTEGER">>),
    ok = liver:add_rule(cov_alias, liver_standard_rules),
    ?assertEqual(liver_standard_rules, liver:which(cov_alias)),
    ok.

%%--------------------------------------------------------------------
%% Helpers: one call checks maps and proplists
%%--------------------------------------------------------------------

ok(Rules, Input, Output) ->
    ok(Rules, Input, Output, #{}).

ok(Rules, Input, Output, Opts) ->
    ?assertEqual({ok, Output}, liver:validate(Rules, Input, Opts)),
    {PlRules, PlInput, PlOutput, PlOpts} = to_proplists(Rules, Input, Output, Opts),
    ?assertEqual({ok, PlOutput}, liver:validate(PlRules, PlInput, PlOpts)).

err(Rules, Input, Errors) ->
    err(Rules, Input, Errors, #{}).

err(Rules, Input, Errors, Opts) ->
    ?assertEqual({error, Errors}, liver:validate(Rules, Input, Opts)),
    {PlRules, PlInput, PlErrors, PlOpts} = to_proplists(Rules, Input, Errors, Opts),
    ?assertEqual({error, PlErrors}, liver:validate(PlRules, PlInput, PlOpts)).

to_proplists(Rules, Input, Result, Opts) ->
    PlResult = case Opts of
        #{return := map} -> Result;
        #{return := proplist} -> Result;
        _ -> result_pl(Rules, Result)
    end,
    {deep_pl(Rules), data_pl(Rules, Input), PlResult, opts_pl(Opts)}.

opts_pl(Opts) ->
    case erlang:is_map(Opts) of
        true -> maps:to_list(Opts);
        false -> Opts
    end.

data_pl(Rules, Term) ->
    case has_nested_map(Rules) of
        true -> deep_pl(Term);
        false -> shell_pl(Term)
    end.

result_pl(Rules, Term) ->
    case has_nested_map(Rules) of
        true -> deep_pl(Term);
        false -> shell_pl(Term)
    end.

shell_pl(Value) ->
    case erlang:is_map(Value) of
        true -> maps:to_list(Value);
        false -> Value
    end.

deep_pl(Value) ->
    case erlang:is_map(Value) of
        true ->
            [{K, deep_pl(V)} || {K, V} <- maps:to_list(Value)];
        false ->
            case erlang:is_list(Value) of
                true ->
                    case Value =/= [] andalso lists:all(fun({_, _}) -> true; (_) -> false end, Value) of
                        true -> [{K, deep_pl(V)} || {K, V} <- Value];
                        false -> [deep_pl(V) || V <- Value]
                    end;
                false ->
                    case erlang:is_tuple(Value) of
                        true ->
                            list_to_tuple([deep_pl(E) || E <- tuple_to_list(Value)]);
                        false ->
                            Value
                    end
            end
    end.

has_nested_map({nested_map, _}) ->
    true;
has_nested_map(Term) ->
    case erlang:is_map(Term) of
        true ->
            lists:any(fun has_nested_map/1, maps:values(Term));
        false ->
            case erlang:is_list(Term) of
                true ->
                    lists:any(fun has_nested_map/1, Term);
                false ->
                    case erlang:is_tuple(Term) of
                        true ->
                            lists:any(fun has_nested_map/1, tuple_to_list(Term));
                        false ->
                            false
                    end
            end
    end.
