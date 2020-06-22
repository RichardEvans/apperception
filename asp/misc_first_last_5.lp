

%------------------------------------------------------------------------------
% We choose whether two adjacent determinations are simultaneous or successive.
%------------------------------------------------------------------------------
1 { sim((BV1, Obj1, ST), (BV2, Obj2, ST+1)); succ((BV1, Obj1, ST), (BV2, Obj2, ST+1)) } 1 :- bv_at(BV1, Obj1, ST), bv_at(BV2, Obj2, ST+1).

% The position in objective time is determined by the sim/succ relations
position((BV, Obj, 1), 1) :- bv_at(BV, Obj, 1).
position(X, T) :- position(Y, T), sim(Y, X).
position(X, T+1) :- 
    position(Y, T), succ(Y, X), max_subjective_time(MT), T+1 <= MT.

% We count the number of subjective time-steps
max_subjective_time(MT) :- is_st(MT), not is_st(MT+1).
is_st(ST) :- bv_at(_, _, ST).
    
%------------------------------------------------------------------------------
% We do not allow two distinct readings from the same object in the same 
% moment of objective time. 
%------------------------------------------------------------------------------
:- position((BV1, Obj, _), T), position((BV2, Obj, _), T), BV1 != BV2.

%------------------------------------------------------------------------------
% We choose a subsumption from one of the predicates allowed by the neural net.
%------------------------------------------------------------------------------
1 { senses(s(C, Obj), T) : possible_pred(BV, C) } 1 :- 
    position((BV, Obj, _), T).

% Possible predicate assignments from bnn
possible_pred(BV, c_p) :- bnn_result(BV, 1, 1).
possible_pred(BV, c_q) :- bnn_result(BV, 2, 1).

%------------------------------------------------------------------------------
% Imagination
%
% If there is a subsumption of X falling under p,
% then there must be an underlying determination det(X, Attr),
% where Attr falls under p.
%------------------------------------------------------------------------------

1 { obj_bv_at(Attr, X, ObjT) : bnn_result(Attr, 1, 1) } 1 :- 
    holds(s(c_p, X), ObjT).

1 { obj_bv_at(Attr, X, ObjT) : bnn_result(Attr, 2, 1) } 1 :- 
    holds(s(c_q, X), ObjT).

obj_bv_at(Attr, X, ObjT) :-
    bv_at(Attr, X, SubjT),
    subj_obj_t(SubjT, ObjT).    

subj_obj_t(SubjT, ObjT) :- position((_, _, SubjT), ObjT).

%------------------------------------------------------------------------------
% We minimise the number of objective time-steps
%------------------------------------------------------------------------------
:~ num_objective_time_steps(T). [T @ 2]

num_objective_time_steps(T) :- is_time(T), not is_time(T+1).

is_time(T) :- senses(_, T).

%------------------------------------------------------------------------------
% We minimise the number of ambiguous vectors assigned to multiple predicates.
%------------------------------------------------------------------------------
:~count_ambiguities(N). [ N @ 3, N]

count_ambiguities(N) :- N = #count { BV : ambiguous(BV) }.

ambiguous(BV) :- 
    possible_pred(BV, C1),
    possible_pred(BV, C2),
    C1 != C2.

%------------------------------------------------------------------------------
% We minimise the number of vectors assigned to the same predicate.
%------------------------------------------------------------------------------
:~ max_bnn_examples_per_predicate(M). [ M @ 4, M]

max_bnn_examples_per_predicate(M) :-
    M = #max { N : count_bnn_examples_per_predicate(C, N) }.

count_bnn_examples_per_predicate(C, N) :-
    possible_pred(_, C),
    N = #count { E : possible_pred(E, C) }.

%------------------------------------------------------------------------------
% Two determinations are incompossible if the attributes fall 
% under predicates that are incompatible.
%------------------------------------------------------------------------------
inc((BV1, Obj, ST1), (BV2, Obj, ST2)) :-
    bv_at(BV1, Obj, ST1), 
    bv_at(BV2, Obj, ST2), 
    possible_pred(BV1, P1),
    possible_pred(BV2, P2),
    not is_ambiguous(BV1),
    not is_ambiguous(BV2),
    incompossible(s(P1, Obj), s(P2, Obj)).

is_ambiguous(BV) :-
    possible_pred(BV, P1),
    possible_pred(BV, P2),
    P1 != P2.



