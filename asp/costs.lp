

%------------------------------------------------------------------------------
% The cost of an interpretation depends on the complexity of the rules.
%
% We count the number of atoms appearing, the 
%------------------------------------------------------------------------------

:~ rule_body(R, A). [1 @ 1, R, A]

:~ rule_arrow_head(R, A). [1 @ 1, R, A]

:~ rule_causes_head(R, A). [1 @ 1, R, A]

:~ init(A). [1 @ 1, A]

:~ gen_permanent(A). [1 @ 1, A]

%------------------------------------------------------------------------------
% For accumulating statistics
%------------------------------------------------------------------------------

used_arrow_rule(R) :-
    is_arrow_rule(R),
    use_rule(R).

used_causes_rule(R) :-
    is_causes_rule(R),
    use_rule(R).


