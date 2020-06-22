

%------------------------------------------------------------------------------
% Sensations need to be connected to holds/2 atoms
% (Intuitions without concepts are blind)
%------------------------------------------------------------------------------

violation_kant_condition_blind_sense :-
    senses(S, T),
    not holds(S, T).

%------------------------------------------------------------------------------
% Aesthetic constraint: spatial unity
%------------------------------------------------------------------------------

violation_kant_condition_spatial_unity :- 
    spatial_unity_counterexample(X, Y, T).

spatial_unity_counterexample(X, Y, T) :-
    permanent(isa(t_object, X)), 
    permanent(isa(t_object, Y)), 
    is_time(T),
    not related(X, Y, T).

related(X, Y, T) :-
    holds(s2(_, X, Y), T).

related(X, Y, T) :-
    permanent(isa2(_, X, Y)),
    is_time(T).

related(X, X, T) :-
    permanent(isa(t_object, X)), 
    is_time(T).
    
related(X, Y, T) :- related(Y, X, T).

related(X, Y, T) :-
    related(X, Z, T),
    related(Z, Y, T).

permanent(isa(t_object, X)) :- is_object(X).

%------------------------------------------------------------------------------
% Aesthetic constraint: part-of must create a labeled pointed graph
%------------------------------------------------------------------------------

violation_kant_condition_spatial_unity :- 
    interpretation_uses_part_of,
    not exists_spatial_unity.

exists_spatial_unity :-
    spatial_unifier(X).

spatial_unifier(X) :-    
    permanent(isa(t_object, X)),
    not spatial_unity_counterexample(X).

spatial_unity_counterexample(X) :-
    permanent(isa(t_object, X)),
    permanent(isa(t_object, Y)),
    X != Y,
    is_time(T),
    not part_of_trans(Y, X, T).

interpretation_uses_part_of :- part_of(_, _, _).

part_of_trans(Part, Whole, T) :-
    part_of(Part, Whole, T).

part_of_trans(X, Y, T) :-
    part_of(X, Z, T),
    part_of_trans(Z, Y, T).

part_of(Part, Whole, T) :-
    holds(s2(c_part, Part, Whole), T).

part_of(Part, Whole, T) :-
    is_time(T),
    permanent(isa2(p_part, Part, Whole)).

violation(spatial_cycle) :-
    part_of_trans(X, Y, T),
    part_of_trans(Y, X, T),
    X != Y.

%------------------------------------------------------------------------------
% Verify judgements.
%
% Every variable group referenced in a judgement must have a corresponding subs
%------------------------------------------------------------------------------

violation(var_group(R, VG)) :-
    rule_var_group(R, VG),
    not has_subs(VG).

has_subs(VG) :- subs_group(VG, _).

% Every variable that appears in a rule must be contained in the rule's group.
violation(undefined_var(R, V)) :-  
    var_in_rule(V, R), 
    rule_var_group(R, VG), 
    not contains_var(VG, V).

%------------------------------------------------------------------------------
% Ordering constraints (symmetry breaking)
%------------------------------------------------------------------------------

violation(bad_var_order(V1, V2, R)) :-
    less_var(V1, V2),
    var_in_body(V2, R),
    not appears_before(V1, V2, R).

violation(bad_rule_order(R1, R2)) :-
    earlier_rule(R1, R2),
    has_rule_body(R1),
    has_rule_body(R2),
    not less_rule(R1, R2).

% Symmetry-breaking constraint for use_rule
:-
    is_arrow_rule(R1),
    is_arrow_rule(R2),
    R1 < R2,
    use_rule(R2),
    not use_rule(R1).

% Symmetry-breaking constraint for use_rule
:-
    is_causes_rule(R1),
    is_causes_rule(R2),
    R1 < R2,
    use_rule(R2),
    not use_rule(R1).

%------------------------------------------------------------------------------
% Correctness
%------------------------------------------------------------------------------

incorrect :-
    hidden(A, T),
    not holds(A, T).

correct :- not incorrect.

%------------------------------------------------------------------------------
% Correctness of held-outs for pixel arrays
%------------------------------------------------------------------------------

incorrect :- incorrect_at(T).

incorrect_at(T) :- 
    test_sprite_at(T, C, S),
    not possible_sprite_at(T, C, S).

incorrect_at(T) :-
    impossible_combination_satisfied(T, N).

impossible_combination_satisfied(T, N) :-
    is_impossible_combination(T, N),
    not unsat(T, N).

unsat(T, N) :-
    test_impossible_combination(T, N, C, S),
    not possible_sprite_at(T, C, S).

is_impossible_combination(T, N) :- test_impossible_combination(T, N, _, _).

%%%

count_num_inaccurate(N) :- 
    N = #count { T : incorrect_at(T) }.

count_num_accurate(X-Y) :- 
    count_num_held_out_time_steps(X), count_num_inaccurate(Y).

count_num_held_out_time_steps(N) :- 
    N = #count { T : is_held_out_time_step(T) }.

is_held_out_time_step(T) :- 
    test_sprite_at(T, _, _).

%------------------------------------------------------------------------------
% Redundant constraints
%------------------------------------------------------------------------------

:-  init(A),
    init(B),
    incompossible(A, B).

:-  gen_permanent(A),
    gen_permanent(B),    
    incompossible(A, B).

:-  rule_body(R, A),
    rule_body(R, B),
    definitely_incompossible_var_atoms(A, B).    

:-  rule_body(R, A),
    rule_arrow_head(R, A).

:-  rule_body(R, A),
    rule_head_causes(R, A).

:-  rule_body(R, A),
    rule_arrow_head(R, B),
    definitely_incompossible_var_atoms(A, B).    

% Unsafe rules
:-  rule_arrow_head(R, s(_, V)),
    not var_in_body(V, R).

:-  rule_arrow_head(R, s2(_, V, _)),
    not var_in_body(V, R).

:-  rule_arrow_head(R, s2(_, _, V)),
    not var_in_body(V, R).

:-  rule_head_causes(R, s(_, V)),
    not var_in_body(V, R).

:-  rule_head_causes(R, s2(_, V, _)),
    not var_in_body(V, R).

:-  rule_head_causes(R, s2(_, _, V)),
    not var_in_body(V, R).

%------------------------------------------------------------------------------
% Prevent general (non-Kantian) violations
%------------------------------------------------------------------------------

:- violation(_).

