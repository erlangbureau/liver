-define(MISSED_FIELD_VALUE, '$missed_field').

%% Aliased rules of a validate call, resolved by liver:normalize_opts/1:
%% #{Name => {Rules, Error}}. Absent from the options if there are none.
-define(ALIASED_RULES_OPT, '$aliased_rules').

%% The containing object of the validated fields (LIVR 2.5, section 2.5): the object
%% that equal_to_field and the references of required_if and when read.
-define(CONTAINER_OPT, '$container').
