
% Inputs to binary neural network
bnn_input(ex_0, node(1, 1), 0).
bnn_input(ex_0, node(1, 2), 0).
bnn_input(ex_0, node(1, 3), 0).
bnn_input(ex_1, node(1, 1), 0).
bnn_input(ex_1, node(1, 2), 0).
bnn_input(ex_1, node(1, 3), 1).
bnn_input(ex_2, node(1, 1), 0).
bnn_input(ex_2, node(1, 2), 1).
bnn_input(ex_2, node(1, 3), 0).
bnn_input(ex_3, node(1, 1), 0).
bnn_input(ex_3, node(1, 2), 1).
bnn_input(ex_3, node(1, 3), 1).
bnn_input(ex_4, node(1, 1), 1).
bnn_input(ex_4, node(1, 2), 0).
bnn_input(ex_4, node(1, 3), 0).
bnn_input(ex_5, node(1, 1), 1).
bnn_input(ex_5, node(1, 2), 0).
bnn_input(ex_5, node(1, 3), 1).
bnn_input(ex_6, node(1, 1), 1).
bnn_input(ex_6, node(1, 2), 1).
bnn_input(ex_6, node(1, 3), 0).
bnn_input(ex_7, node(1, 1), 1).
bnn_input(ex_7, node(1, 2), 1).
bnn_input(ex_7, node(1, 3), 1).
% Inputs to binary neural network
bnn_input(ex_0, node(1, 1), 0).
bnn_input(ex_0, node(1, 2), 0).
bnn_input(ex_0, node(1, 3), 0).
bnn_input(ex_1, node(1, 1), 0).
bnn_input(ex_1, node(1, 2), 0).
bnn_input(ex_1, node(1, 3), 1).
bnn_input(ex_2, node(1, 1), 0).
bnn_input(ex_2, node(1, 2), 1).
bnn_input(ex_2, node(1, 3), 0).
bnn_input(ex_3, node(1, 1), 0).
bnn_input(ex_3, node(1, 2), 1).
bnn_input(ex_3, node(1, 3), 1).
bnn_input(ex_4, node(1, 1), 1).
bnn_input(ex_4, node(1, 2), 0).
bnn_input(ex_4, node(1, 3), 0).
bnn_input(ex_5, node(1, 1), 1).
bnn_input(ex_5, node(1, 2), 0).
bnn_input(ex_5, node(1, 3), 1).
bnn_input(ex_6, node(1, 1), 1).
bnn_input(ex_6, node(1, 2), 1).
bnn_input(ex_6, node(1, 3), 0).
bnn_input(ex_7, node(1, 1), 1).
bnn_input(ex_7, node(1, 2), 1).
bnn_input(ex_7, node(1, 3), 1).

%--------------------------------------------------------
%   Time            Obj     
%                           
%   1               off     
%   2               off     
%   3               on     
%   4               off     
%   5               on     
%   6               on     
%   7               on      
%   8               off     
%   9               off     
%   10              on     
%   11              off     
%   12              on      
%   13              off     
%   14              on      
%   15              off     
%   16              off     
%--------------------------------------------------------

% The given sequence
bv_at(ex_0, obj_s, 1).
bv_at(ex_3, obj_s, 2).
bv_at(ex_6, obj_s, 3).
bv_at(ex_1, obj_s, 4).
bv_at(ex_7, obj_s, 5).
bv_at(ex_5, obj_s, 6).
bv_at(ex_6, obj_s, 7).
bv_at(ex_1, obj_s, 8).
bv_at(ex_2, obj_s, 9).
bv_at(ex_4, obj_s, 10).
bv_at(ex_0, obj_s, 11).
bv_at(ex_7, obj_s, 12).
bv_at(ex_3, obj_s, 13).
bv_at(ex_5, obj_s, 14).
bv_at(ex_0, obj_s, 15).
bv_at(ex_2, obj_s, 16).



% Exogenous
exogenous(s2(c_value, obj_a, obj_value_1), 1).
exogenous(s2(c_value, obj_b, obj_value_2), 1).
exogenous(s2(c_value, obj_a, obj_value_4), 2).
exogenous(s2(c_value, obj_b, obj_value_9), 2).
exogenous(s2(c_value, obj_a, obj_value_4), 3).
exogenous(s2(c_value, obj_b, obj_value_2), 3).
exogenous(s2(c_value, obj_a, obj_value_1), 4).
exogenous(s2(c_value, obj_b, obj_value_2), 4).
exogenous(s2(c_value, obj_a, obj_value_7), 5).
exogenous(s2(c_value, obj_b, obj_value_2), 5).
exogenous(s2(c_value, obj_a, obj_value_8), 6).
exogenous(s2(c_value, obj_b, obj_value_5), 6).
exogenous(s2(c_value, obj_a, obj_value_9), 7).
exogenous(s2(c_value, obj_b, obj_value_3), 7).
exogenous(s2(c_value, obj_a, obj_value_7), 8).
exogenous(s2(c_value, obj_b, obj_value_9), 8).
exogenous(s2(c_value, obj_a, obj_value_3), 9).
exogenous(s2(c_value, obj_b, obj_value_8), 9).
exogenous(s2(c_value, obj_a, obj_value_7), 10).
exogenous(s2(c_value, obj_b, obj_value_4), 10).
exogenous(s2(c_value, obj_a, obj_value_5), 11).
exogenous(s2(c_value, obj_b, obj_value_6), 11).
exogenous(s2(c_value, obj_a, obj_value_9), 12).
exogenous(s2(c_value, obj_b, obj_value_6), 12).
exogenous(s2(c_value, obj_a, obj_value_1), 13).
exogenous(s2(c_value, obj_b, obj_value_2), 13).
exogenous(s2(c_value, obj_a, obj_value_4), 14).
exogenous(s2(c_value, obj_b, obj_value_1), 14).
exogenous(s2(c_value, obj_a, obj_value_1), 15).
exogenous(s2(c_value, obj_b, obj_value_5), 15).
exogenous(s2(c_value, obj_a, obj_value_3), 16).
exogenous(s2(c_value, obj_b, obj_value_6), 16).


% Elements
is_object(obj_s).
is_object(obj_a).
is_object(obj_b).
is_concept(c_p).
is_concept(c_q).
is_concept(c_value).
is_time(1..16).

% Input exclusions
% Every object is either p or q
% p(X) ⊕ q(X)
% Input exclusions
% Every sensor is either p or q
% S : sensor → p(S) ⊕ q(S)

% At most one
:-
    holds(s(c_p, X), T),
    holds(s(c_q, X), T).

% At least one
:-
    permanent(isa(t_sensor, X)),
    is_time(T),
    not holds(s(c_p, X), T),
    not holds(s(c_q, X), T).

% Incompossibility
incompossible(s(c_p, X), s(c_q, X)) :-
    permanent(isa(t_sensor, X)).

exclusion_output("c_p+c_q").

% Every cell has a unique value attribute

% ∃! clause for c_value : at most one

:-
    holds(s2(c_value, X, Y), T), 
    holds(s2(c_value, X, Y2), T), 
    Y != Y2.

% ∃! clause for c_value : at least one

:-
    permanent(isa(t_cell, X)),
    is_time(T),
    not aux_c_value(X, T).

aux_c_value(X, T) :-
    holds(s2(c_value, X, _), T).

% Incompossibility for c_value
incompossible(s2(c_value, X, Y), s2(c_value, X, Y2)) :-
    permanent(isa(t_cell, X)),
    permanent(isa(t_value, Y)),
    permanent(isa(t_value, Y2)),
    Y != Y2.



%------------------------------------------------------------------------------
% Permanents 
%------------------------------------------------------------------------------

permanent(isa2(p_left, obj_s, obj_a)).
permanent(isa2(p_right, obj_s, obj_b)).

succ(obj_value_1, obj_value_2).
succ(obj_value_2, obj_value_3).
succ(obj_value_3, obj_value_4).
succ(obj_value_4, obj_value_5).
succ(obj_value_5, obj_value_6).
succ(obj_value_6, obj_value_7).
succ(obj_value_7, obj_value_8).
succ(obj_value_8, obj_value_9).

permanent(isa2(p_greater, X, Y)) :- succ(Y, X).

permanent(isa2(p_greater, X, Y)) :- succ(Z, X), permanent(isa2(p_greater, Z, Y)).

%------------------------------------------------------------------------------
% Binary neural network
%------------------------------------------------------------------------------

nodes(1, 3).
nodes(2, 2).
nodes(3, 2).

% Possible predicate assignments from bnn
possible_pred(BV, c_p) :- bnn_result(BV, 1, 1).
possible_pred(BV, c_q) :- bnn_result(BV, 2, 1).

% Senses choice from bnn assignments
1 { senses(s(C, Obj), T) : possible_pred(BV, C) } 1 :- bv_at(BV,Obj,T).

is_possible_pred(C) :- possible_pred(_, C).

count_bnn_examples_per_predicate(C, N) :-
    is_possible_pred(C),
    N = #count { E : possible_pred(E, C) }.

num_bvs(N) :- N = #count { BV : bv_at(T, BV), is_time(T) }.

max_bnn_examples_per_predicate(M) :-
    M = #max { N : count_bnn_examples_per_predicate(C, N) }.

:~ max_bnn_examples_per_predicate(M). [ M @ 2, M]