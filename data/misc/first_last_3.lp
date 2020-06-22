% No time abduction, noisy input.

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

% We shall say:
% off is either [0,0,0], [0,0,1,], [0,1,0], [0,1,1]
% on is either [0,1,1], [1,0,0], [1,0,1,], [1,1,0], [1,1,1]
% 
% Note that [0,1,1] is ambiguous

% The given sequence
bv_at(ex_4, obj_cell_1, 1).
bv_at(ex_5, obj_cell_2, 1).
bv_at(ex_1, obj_cell_1, 2).
bv_at(ex_5, obj_cell_2, 2).
bv_at(ex_4, obj_cell_1, 3).
bv_at(ex_0, obj_cell_2, 3).
bv_at(ex_5, obj_cell_1, 4).
bv_at(ex_4, obj_cell_2, 4).
bv_at(ex_2, obj_cell_1, 5).
bv_at(ex_4, obj_cell_2, 5).
bv_at(ex_5, obj_cell_1, 6).
bv_at(ex_1, obj_cell_2, 6).
bv_at(ex_7, obj_cell_1, 7).
bv_at(ex_6, obj_cell_2, 7).
bv_at(ex_0, obj_cell_1, 8).
bv_at(ex_3, obj_cell_2, 8).
bv_at(ex_4, obj_cell_1, 9).
bv_at(ex_3, obj_cell_2, 9).
bv_at(ex_5, obj_cell_1, 10).
bv_at(ex_4, obj_cell_2, 10).
bv_at(ex_2, obj_cell_1, 11).
bv_at(ex_6, obj_cell_2, 11).
bv_at(ex_4, obj_cell_1, 12).
bv_at(ex_1, obj_cell_2, 12).
bv_at(ex_3, obj_cell_1, 13).
bv_at(ex_4, obj_cell_2, 13).
bv_at(ex_0, obj_cell_1, 14).
bv_at(ex_7, obj_cell_2, 14).
bv_at(ex_4, obj_cell_1, 15).
bv_at(ex_1, obj_cell_2, 15).

% Elements
is_object(obj_cell_1).
is_object(obj_cell_2).
is_time(1..15).
is_concept(c_on).
is_concept(c_off).

% Input exclusions
% Every sensor is either on or off
% S : sensor → on(S) ⊕ off(S)

% At most one
:-
    holds(s(c_on, X), T),
    holds(s(c_off, X), T).

% At least one
:-
    permanent(isa(t_sensor, X)),
    is_time(T),
    not holds(s(c_on, X), T),
    not holds(s(c_off, X), T).

% Incompossibility
incompossible(s(c_on, X), s(c_off, X)) :-
    permanent(isa(t_sensor, X)).

% Binary neural network
nodes(1, 3).
nodes(2, 2).
nodes(3, 2).

% Possible predicate assignments from bnn
possible_pred(BV, c_on) :- bnn_result(BV, 1, 1).
possible_pred(BV, c_off) :- bnn_result(BV, 2, 1).

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