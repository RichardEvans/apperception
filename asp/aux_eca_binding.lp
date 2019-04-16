

% Both black and white are derived predicates.
%
% This means they cannot feature in inits or in the heads of causes rules.

:- init(s(c_black, _)).
:- init(s(c_white, _)).

:- rule_head_causes(_, s(c_black, _)).

:- rule_head_causes(_, s(c_white, _)).

% The touch/2 predicate is similarly derived.

:- init(s2(c_touch, _, _)).

:- rule_head_causes(_, s2(c_touch, _, _)).
