%% Case names for erlang_standard_SUITE.
-define(ERLANG_STANDARD_POSITIVE_CASES, [
    is_integer,
    is_string,
    is_utf8_binary,
    is_atom,
    is_float,
    is_number,
    is_non_neg_integer,
    is_pos_integer,
    to_integer,
    to_float,
    to_utf8_binary,
    to_existing_atom,
    to_map,
    to_proplist,
    one_of_terms,
    range,
    byte_size,
    length,
    is_tuple,
    email_ascii,
    email_unicode,
    iso_date_binary,
    iso_date_tuple,
    nested_map,
    rule_set_livr_spec,
    rule_set_compose,
    rule_set_mixed_alias
]).

-define(ERLANG_STANDARD_NEGATIVE_CASES, [
    is_integer,
    is_string,
    is_non_neg_integer,
    to_existing_atom,
    one_of_terms,
    range,
    email,
    iso_date,
    nested_map,
    livr_name_unavailable,
    is_integer_under_livr_spec
]).
