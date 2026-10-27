-define(MISSED_FIELD_VALUE, '$missed_field').

%% Aliased rules of a validate call, resolved by liver:normalize_opts/1:
%% #{Name => {Rules, Error}}. Absent from the options if there are none.
-define(ALIASED_RULES_OPT, '$aliased_rules').
