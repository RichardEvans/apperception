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
bv_at(ex_5, obj_cell_2, 2).
bv_at(ex_1, obj_cell_1, 3).
bv_at(ex_5, obj_cell_2, 4).
bv_at(ex_0, obj_cell_2, 5).
bv_at(ex_4, obj_cell_1, 6).
bv_at(ex_5, obj_cell_1, 7).
bv_at(ex_4, obj_cell_2, 8).
bv_at(ex_4, obj_cell_2, 9).
bv_at(ex_2, obj_cell_1, 10).
bv_at(ex_5, obj_cell_1, 11).
bv_at(ex_1, obj_cell_2, 12).
bv_at(ex_7, obj_cell_1, 13).
bv_at(ex_6, obj_cell_2, 14).
bv_at(ex_0, obj_cell_1, 15).
bv_at(ex_3, obj_cell_2, 16).
bv_at(ex_3, obj_cell_2, 17).
bv_at(ex_4, obj_cell_1, 18).
bv_at(ex_5, obj_cell_1, 19).
bv_at(ex_4, obj_cell_2, 20).
bv_at(ex_6, obj_cell_2, 21).
bv_at(ex_2, obj_cell_1, 22).
bv_at(ex_4, obj_cell_1, 23).
bv_at(ex_1, obj_cell_2, 24).
bv_at(ex_3, obj_cell_1, 25).
bv_at(ex_4, obj_cell_2, 26).
bv_at(ex_0, obj_cell_1, 27).
bv_at(ex_7, obj_cell_2, 28).
bv_at(ex_1, obj_cell_2, 29).
bv_at(ex_4, obj_cell_1, 30).
bv_at(ex_3, obj_cell_2, 31).
bv_at(ex_7, obj_cell_1, 32).
bv_at(ex_3, obj_cell_1, 33).
bv_at(ex_7, obj_cell_2, 34).
bv_at(ex_2, obj_cell_2, 35).
bv_at(ex_7, obj_cell_1, 36).
bv_at(ex_6, obj_cell_1, 37).
bv_at(ex_5, obj_cell_2, 38).
bv_at(ex_4, obj_cell_2, 39).
bv_at(ex_2, obj_cell_1, 40).
bv_at(ex_5, obj_cell_1, 41).
bv_at(ex_1, obj_cell_2, 42).
bv_at(ex_6, obj_cell_2, 43).
bv_at(ex_3, obj_cell_1, 44).
bv_at(ex_2, obj_cell_1, 45).
bv_at(ex_7, obj_cell_2, 46).
bv_at(ex_3, obj_cell_2, 47).
bv_at(ex_7, obj_cell_1, 48).
bv_at(ex_6, obj_cell_1, 49).
bv_at(ex_4, obj_cell_2, 50).

% Elements
is_object(obj_cell_1).
is_object(obj_cell_2).
%is_time(1..15).
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
1 { given(s(C, Obj), T) : possible_pred(BV, C) } 1 :- bv_at(BV,Obj,T).

is_possible_pred(C) :- possible_pred(_, C).

count_bnn_examples_per_predicate(C, N) :-
    is_possible_pred(C),
    N = #count { E : possible_pred(E, C) }.

num_bvs(N) :- N = #count { BV : bv_at(T, BV), is_time(T) }.

max_bnn_examples_per_predicate(M) :-
    M = #max { N : count_bnn_examples_per_predicate(C, N) }.

:~ max_bnn_examples_per_predicate(M). [ M @ 8, M]