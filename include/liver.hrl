-define(DEFAULT_RULES, #{
    %% LIVR common rules
    required                    => liver_livr_rules,
    not_empty                   => liver_livr_rules,
    not_empty_list              => liver_livr_rules,
    any_object                  => liver_livr_rules,

    %% LIVR string rules
    string                      => liver_livr_rules,
    eq                          => liver_livr_rules,
    one_of                      => liver_livr_rules,
    subset                      => liver_livr_rules,
    max_length                  => liver_livr_rules,
    min_length                  => liver_livr_rules,
    length_between              => liver_livr_rules,
    length_equal                => liver_livr_rules,
    like                        => liver_livr_rules,

    %% LIVR numeric rules
    integer                     => liver_livr_rules,
    positive_integer            => liver_livr_rules,
    decimal                     => liver_livr_rules,
    positive_decimal            => liver_livr_rules,
    max_number                  => liver_livr_rules,
    min_number                  => liver_livr_rules,
    number_between              => liver_livr_rules,

    %% LIVR special rules
    email                       => liver_livr_rules,
    url                         => liver_livr_rules,
    iso_date                    => liver_livr_rules,
    equal_to_field              => liver_livr_rules,

    %% LIVR meta rules
    nested_object               => liver_livr_rules,
    variable_object             => liver_livr_rules,
    list_of                     => liver_livr_rules,
    list_of_objects             => liver_livr_rules,
    list_of_different_objects   => liver_livr_rules,
    'or'                        => liver_livr_rules,

    %% LIVR modifiers (previously - "filter rules")
    trim                        => liver_livr_rules,
    to_lc                       => liver_livr_rules,
    to_uc                       => liver_livr_rules,
    remove                      => liver_livr_rules,
    leave_only                  => liver_livr_rules,
    default                     => liver_livr_rules,

    %% liver strict rules
    is_null                     => liver_strict_rules,
    is_undefined                => liver_strict_rules,
    is_integer                  => liver_strict_rules,
    is_boolean                  => liver_strict_rules,
    is_list                     => liver_strict_rules,
    is_string                   => liver_strict_rules,
    is_bstring                  => liver_strict_rules,
    is_atom                     => liver_strict_rules,
    to_integer                  => liver_strict_rules,
    to_boolean                  => liver_strict_rules
}).

-define(DEFAULT_ERRORS, #{
    required                => <<"REQUIRED">>,
    format_error            => <<"FORMAT_ERROR">>,
    cannot_be_empty         => <<"CANNOT_BE_EMPTY">>,
    too_long                => <<"TOO_LONG">>,
    too_short               => <<"TOO_SHORT">>,
    too_high                => <<"TOO_HIGH">>,
    too_low                 => <<"TOO_LOW">>,
    not_allowed_value       => <<"NOT_ALLOWED_VALUE">>,
    not_number              => <<"NOT_NUMBER">>,
    not_integer             => <<"NOT_INTEGER">>,
    not_positive_integer    => <<"NOT_POSITIVE_INTEGER">>,
    not_decimal             => <<"NOT_DECIMAL">>,
    not_positive_decimal    => <<"NOT_POSITIVE_DECIMAL">>,
    wrong_format            => <<"WRONG_FORMAT">>,
    wrong_date              => <<"WRONG_DATE">>,
    wrong_email             => <<"WRONG_EMAIL">>,
    wrong_url               => <<"WRONG_URL">>,
    fields_not_equal        => <<"FIELDS_NOT_EQUAL">>,
    unknown_field           => <<"UNKNOWN_FIELD">>
}).
