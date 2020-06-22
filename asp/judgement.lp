%------------------------------------------------------------------------------
% eval
%------------------------------------------------------------------------------

eval_body(R, Subs, T) :-
    rule_subs(R, Subs),
    is_time(T),
    eval_atom(V, Subs, T) : rule_body(R, V).

eval_atom(VA, Subs, T) :-
    ground_atom(VA, GA, Subs),
    holds(GA, T).

eval_atom(VA, Subs, T) :-
    ground_atom(VA, GA, Subs),
    permanent(GA),
    is_time(T).

ground_atom(s(C, V), s(C, Obj), Subs) :-
    is_var_fluent(s(C, V)),
    subs(Subs, V, Obj).

ground_atom(s2(C, V, V2), s2(C, Obj, Obj2), Subs) :-
    is_var_fluent(s2(C, V, V2)),
    subs(Subs, V, Obj),
    subs(Subs, V2, Obj2).

ground_atom(isa(C, V), isa(C, Obj), Subs) :-
    is_var_permanent(isa(C, V)),
    subs(Subs, V, Obj).

ground_atom(isa2(C, V, V2), isa2(C, Obj, Obj2), Subs) :-
    is_var_permanent(isa2(C, V, V2)),
    subs(Subs, V, Obj),
    subs(Subs, V2, Obj2).

%------------------------------------------------------------------------------
% holds
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
    not is_static_atom(S),
    not -holds(S, T+1).

-holds(S, T) :-
    holds(S2, T),
    incompossible(S, S2).

% causes update
holds(GC, T+1) :-     
    rule_head_causes(R, VC),
    eval_body(R, Subs, T),
    ground_atom(VC, GC, Subs),
    is_time(T+1).

% arrow update
holds(GA, T) :-
    rule_subs(R, Subs),
    rule_arrow_head(R, VA),
    ground_atom(VA, GA, Subs),
    eval_body(R, Subs, T).

% exogenous action
holds(GA, T) :- exogenous(GA, T).

%------------------------------------------------------------------------------
% Utility functions
%------------------------------------------------------------------------------

is_rule(R) :-
    rule_var_group(R, _).

rule_subs(R, Subs) :-
    rule_var_group(R, VG),
    subs_group(VG, Subs).

atom_in_rule(A, R) :- 
    rule_body(R, A).

atom_in_rule(A, R) :- 
    rule_arrow_head(R, A).

atom_in_rule(A, R) :- 
    rule_causes_head(R, A).

is_var(V) :- 
    subs(_, V, _).

var_in_rule(V, R) :-
     atom_in_rule(s(_, V), R).

var_in_rule(V, R) :-
     atom_in_rule(s2(_, V, _), R).

var_in_rule(V, R) :-
     atom_in_rule(s2(_, _, V), R).

var_in_rule(V, R) :-
     atom_in_rule(isa(_, V), R).

var_in_rule(V, R) :-
     atom_in_rule(isa2(_, V, _), R).

var_in_rule(V, R) :-
     atom_in_rule(isa2(_, _, V), R).

is_var_group(VG) :- 
    subs_group(VG, _).

is_gen_rule(R) :- is_causes_rule(R).
is_gen_rule(R) :- is_arrow_rule(R).

is_var_fluent(S) :- var_fluent(S, _).

is_var_permanent(S) :- var_permanent(S, _).

is_var_atom(S) :- is_var_fluent(S).
is_var_atom(S) :- is_var_permanent(S).

var_atom_group(VA, VG) :- var_fluent(VA, VG).    
var_atom_group(VA, VG) :- var_permanent(VA, VG).    

cause_head(s(C, V)) :-
    is_var_fluent(s(C, V)),
    not is_static_concept(C).

cause_head(s2(C, V, V2)) :-
    is_var_fluent(s2(C, V, V2)),
    not is_static_concept(C).

permanent(A) :- gen_permanent(A).

rule_causes_head(R, VC) :-
    rule_head_causes(R, VC).

is_ground_atom(G) :- ground_atom(_, G, _).

is_static_atom(s(C, X)) :-
    is_ground_atom(s(C, X)),
    is_static_concept(C).

is_static_atom(s2(C, X, Y)) :-
    is_ground_atom(s2(C, X, Y)),
    is_static_concept(C).

%------------------------------------------------------------------------------
% Incompossibility
%------------------------------------------------------------------------------

incompossible(X, Y) :- 
    incompossible(Y, X).

% Two var atoms are potentially incompossible
% if there is *some* substitution on which their groundings are incompossible.
potentially_incompossible_var_atoms(VA, VA2) :-
    ground_atom(VA, GA, Subs),    
    ground_atom(VA2, GA2, Subs),
    incompossible(GA, GA2).    

% This predicate is true if the var atoms are incompossible under
% *every* substitution (not just one).
%
% This only applies to unary var atoms.
definitely_incompossible_var_atoms(s(p, V), s(q, V)) :-
    is_var_atom(s(p, V)),
    is_var_atom(s(q, V)),
    incompatible_unary_predicates(p, q).

incompatible_unary_predicates(P, Q) :- 
    incompatible_unary_predicates(Q, P).

%------------------------------------------------------------------------------
% Pure concepts
%------------------------------------------------------------------------------

is_concept(C) :- is_type(C).

% NB t_object means external substance (i.e. situated in space)
is_type(t_object).

%------------------------------------------------------------------------------
% Distinguished time-steps
%------------------------------------------------------------------------------

init_time(T) :- 
    is_time(T),    
    not is_time(T-1).

last_time(T) :- 
    is_time(T),    
    not is_time(T+1).

%------------------------------------------------------------------------------
% Sub-types
%------------------------------------------------------------------------------

permanent(isa(T, X)) :-
    permanent(isa(T2, X)),
    sub_type(T2, T).
    
%------------------------------------------------------------------------------
% Var ordering
%------------------------------------------------------------------------------

less_var(V1, V2) :-
    var_type(V1, T),
    var_type(V2, T),
    V1 < V2.

appears_before(V1, V2, R) :-
    rule_body(R, s(_, V1)),
    var_in_body(V2, R).

appears_before(V1, V2, R) :-
    rule_body(R, s2(_, V1, V3)),
    var_in_body(V2, R),
    V2 != V3.

appears_before(V1, V2, R) :-
    rule_body(R, s2(_, V3, V1)),
    var_in_body(V2, R),
    V2 != V3.

appears_before(V1, V2, R) :-
    rule_body(R, s2(_, V1, V2)).

appears_before(V1, V2, R) :-
    rule_body(R, isa(_, V1)),
    var_in_body(V2, R).

appears_before(V1, V2, R) :-
    rule_body(R, isa2(_, V1, V3)),
    var_in_body(V2, R),
    V2 != V3.

appears_before(V1, V2, R) :-
    rule_body(R, isa2(_, V3, V1)),
    var_in_body(V2, R),
    V2 != V3.

appears_before(V1, V2, R) :-
    rule_body(R, isa2(_, V1, V2)).

var_in_body(V, R) :-
    rule_body(R, s(_, V)).

var_in_body(V, R) :-
    rule_body(R, s2(_, V, _)).

var_in_body(V, R) :-
    rule_body(R, s2(_, _, V)).

var_in_body(V, R) :-
    rule_body(R, isa(_, V)).

var_in_body(V, R) :-
    rule_body(R, isa2(_, V, _)).

var_in_body(V, R) :-
    rule_body(R, isa2(_, _, V)).

%------------------------------------------------------------------------------
% Var-atom ordering
%------------------------------------------------------------------------------

less_var_atom(X, Y) :- 
    less_var_atom(X, Z),
    less_var_atom(Z, Y).

less_var_atom(s(C, V), s2(C2, V2, V3)) :-
    is_var_atom(s(C, V)),
    is_var_atom(s2(C2, V2, V3)).

less_var_atom(s2(C, V, V2), isa(C2, V3)) :-
    is_var_atom(s2(C, V, V2)),
    is_var_atom(isa(C2, V3)).

less_var_atom(isa(C, V), isa2(C2, V2, V3)) :-
    is_var_atom(isa(C, V)),
    is_var_atom(isa2(C2, V2, V3)).

less_var_atom(s(C, V), s(C2, V2)) :-
    is_var_atom(s(C, V)),
    is_var_atom(s(C2, V2)),
    C < C2.

less_var_atom(s(C, V), s(C, V2)) :-
    is_var_atom(s(C, V)),
    is_var_atom(s(C, V2)),
    V < V2.

less_var_atom(s2(C, V, V2), s2(C2, V3, V4)) :-
    is_var_atom(s2(C, V, V2)),
    is_var_atom(s2(C2, V3, V4)),
    C < C2.

less_var_atom(s2(C, V, V2), s2(C, V3, V4)) :-
    is_var_atom(s2(C, V, V2)),
    is_var_atom(s2(C, V3, V4)),
    V < V3.

less_var_atom(s2(C, V, V2), s2(C, V, V3)) :-
    is_var_atom(s2(C, V, V2)),
    is_var_atom(s2(C, V, V3)),
    V2 < V3.

less_var_atom(isa(C, V), isa(C2, V2)) :-
    is_var_atom(isa(C, V)),
    is_var_atom(isa(C2, V2)),
    C < C2.

less_var_atom(isa(C, V), isa(C, V2)) :-
    is_var_atom(isa(C, V)),
    is_var_atom(isa(C, V2)),
    V < V2.

less_var_atom(isa2(C, V, V2), isa2(C2, V3, V4)) :-
    is_var_atom(isa2(C, V, V2)),
    is_var_atom(isa2(C2, V3, V4)),
    C < C2.

less_var_atom(isa2(C, V, V2), isa2(C, V3, V4)) :-
    is_var_atom(isa2(C, V, V2)),
    is_var_atom(isa2(C, V3, V4)),
    V < V3.

less_var_atom(isa2(C, V, V2), isa2(C, V, V3)) :-
    is_var_atom(isa2(C, V, V2)),
    is_var_atom(isa2(C, V, V3)),
    V2 < V3.

less_rule(R1, R2) :-
    is_rule(R1),
    is_rule(R2),
    rule_body(R1, V),
    { less_var_atom(V, V2) : rule_body(R2, V2) }.

has_rule_body(R) :-
    rule_body(R, _).
    
less_rule(X, Y) :-
    less_rule(X, Z),
    less_rule(Z, Y).

earlier_rule(R1, R2) :-
    is_arrow_rule(R1),
    is_arrow_rule(R2),
    R1 < R2.

earlier_rule(R1, R2) :-
    is_causes_rule(R1),
    is_causes_rule(R2),
    R1 < R2.



