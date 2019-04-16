%------------------------------------------------------------------------------
% constraints
%------------------------------------------------------------------------------

:- violation(_).

%------------------------------------------------------------------------------
% Sensations need to be connected to holds/2 atoms
% (Intuitions without concepts are blind)
%------------------------------------------------------------------------------

violation(blind_sense(S, T)) :-
    senses(S, T),
    not holds(S, T).

%------------------------------------------------------------------------------
% Aesthetic constraint: spatial unity
%------------------------------------------------------------------------------

violation(spatial_unity) :- not exists_spatial_unity.

exists_spatial_unity :-
    permanent(isa(t_object, X)),
    not spatial_unity_counterexample(X).

spatial_unity_counterexample(X) :-
    permanent(isa(t_object, X)),
    permanent(isa(t_object, Y)),
    X != Y,
    is_time(T),
    not part_of_trans(Y, X, T).

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
    is_time(T),
    part_of_trans(X, X, T),
    part_of_trans(Y, Y, T),
    X != Y.

%------------------------------------------------------------------------------
% judgement
%------------------------------------------------------------------------------

holds(s(C, Obj), T) :-
    init(s(C, Obj)),
    init_time(T).

holds(s2(C, Obj, Obj2), T) :-
    init(s2(C, Obj, Obj2)),
    init_time(T).
    
% frame axiom
holds(S, T+1) :-
    holds(S, T),
    is_time(T+1),
    not not_holds(S, T+1).

not_holds(S, T) :-
    holds(S2, T),
    incompossible(S, S2).

:- holds(S, T), not_holds(S, T).   

% ILASP-specific causal update rule
holds(S1, T+1) :- 
    causes(S1, S2, T),
    holds(S2, T),
    is_time(T+1).

% ILASP-specific arrow update rule
holds(S, T) :- 
    arrow(S, T).

% utility

permanent(A) :- gen_permanent(A).

incompossible(X, Y) :- 
    incompossible(Y, X).

init_time(T) :- 
    is_time(T),    
    not is_time(T-1).

last_time(T) :- 
    is_time(T),    
    not is_time(T+1).

permanent(isa(T, X)) :-
    permanent(isa(T2, X)),
    sub_type(T2, T).
