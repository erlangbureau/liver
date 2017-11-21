-define(DEFAULT_RULES, #{
    %% common rules
    required                    => oliver_livr_rules,
    not_empty                   => oliver_livr_rules,
    not_empty_list              => oliver_livr_rules,
    any_object                  => oliver_livr_rules,

    %% string rules
    string                      => oliver_livr_rules,
    eq                          => oliver_livr_rules,
    one_of                      => oliver_livr_rules,
    max_length                  => oliver_livr_rules,
    min_length                  => oliver_livr_rules,
    length_between              => oliver_livr_rules,
    length_equal                => oliver_livr_rules,
    like                        => oliver_livr_rules,

    %% numeric rules
    integer                     => oliver_livr_rules,
    positive_integer            => oliver_livr_rules,
    decimal                     => oliver_livr_rules,
    positive_decimal            => oliver_livr_rules,
    max_number                  => oliver_livr_rules,
    min_number                  => oliver_livr_rules,
    number_between              => oliver_livr_rules,

    %% special rules
    email                       => oliver_livr_rules,
    url                         => oliver_livr_rules,
    iso_date                    => oliver_livr_rules,
    equal_to_field              => oliver_livr_rules,

    %% meta rules
    nested_object               => oliver_livr_rules,
    list_of                     => oliver_livr_rules,
    list_of_objects             => oliver_livr_rules,
    list_of_different_objects   => oliver_livr_rules,
    'or'                        => oliver_livr_rules,

    %% modifiers (previously - "filter rules")
    trim                        => oliver_livr_rules,
    to_lc                       => oliver_livr_rules,
    to_uc                       => oliver_livr_rules,
    remove                      => oliver_livr_rules,
    leave_only                  => oliver_livr_rules,
    default                     => oliver_livr_rules
}).
