%--------------------------------------------------
%
% Generated by NoisySequences.hs
%
% Original sequence: 001100110011001100110011001100110011
%
% True bit vector size: 2
%
% Num ambiguities: 1
%
% Bit vector: 010001100101101000011010010011100001111000011110010010100100...
%
% Guessed bit vector size: 2
%
% Guessed num predicates: 3
%
% (Guesses are wrong)
%
% Sequence:
% Time 1: 01
% Time 2: 00
% Time 3: 01
% Time 4: 10
% Time 5: 01
% Time 6: 01
% Time 7: 10
% Time 8: 10
% Time 9: 00
% Time 10: 01
% Time 11: 10
% Time 12: 10
% Time 13: 01
% Time 14: 00
% Time 15: 11
% Time 16: 10
% Time 17: 00
% Time 18: 01
% Time 19: 11
% Time 20: 10
% Time 21: 00
% Time 22: 01
% Time 23: 11
% Time 24: 10
% Time 25: 01
% Time 26: 00
% Time 27: 10
% Time 28: 10
% Time 29: 01
% Time 30: 00
% Time 31: 10
% Time 32: 11
% Time 33: 00
% Time 34: 00
% Time 35: 11
% Time 36: 10
%--------------------------------------------------

% Inputs to binary neural network
bnn_input(ex_0, node(1, 1), 0).
bnn_input(ex_0, node(1, 2), 0).
bnn_input(ex_1, node(1, 1), 0).
bnn_input(ex_1, node(1, 2), 1).
bnn_input(ex_2, node(1, 1), 1).
bnn_input(ex_2, node(1, 2), 0).
bnn_input(ex_3, node(1, 1), 1).
bnn_input(ex_3, node(1, 2), 1).

% Sequence
bv_at(1, ex_1).
bv_at(2, ex_0).
bv_at(3, ex_1).
bv_at(4, ex_2).
bv_at(5, ex_1).
bv_at(6, ex_1).
bv_at(7, ex_2).
bv_at(8, ex_2).
bv_at(9, ex_0).
bv_at(10, ex_1).
bv_at(11, ex_2).
bv_at(12, ex_2).
bv_at(13, ex_1).
bv_at(14, ex_0).
bv_at(15, ex_3).
bv_at(16, ex_2).
bv_at(17, ex_0).
bv_at(18, ex_1).
bv_at(19, ex_3).
bv_at(20, ex_2).
bv_at(21, ex_0).
bv_at(22, ex_1).
bv_at(23, ex_3).
bv_at(24, ex_2).
bv_at(25, ex_1).
bv_at(26, ex_0).
bv_at(27, ex_2).
bv_at(28, ex_2).
bv_at(29, ex_1).
bv_at(30, ex_0).
bv_at(31, ex_2).
bv_at(32, ex_3).
bv_at(33, ex_0).

is_visual_pred(c_1).
is_visual_pred(c_2).
is_visual_pred(c_3).
% acceptable at (34): [False,False]
% acceptable at (35): [True,False]
% acceptable at (35): [True,True]
% acceptable at (36): [True,False]
% acceptable at (36): [True,True]
incorrect :- holds(s(P, obj_sensor), 34), is_visual_pred(P), not possible_pred(ex_0, P).
incorrect :- holds(s(P, obj_sensor), 35), is_visual_pred(P), not possible_pred(ex_2, P).
incorrect :- holds(s(P, obj_sensor), 35), is_visual_pred(P), not possible_pred(ex_3, P).
incorrect :- holds(s(P, obj_sensor), 36), is_visual_pred(P), not possible_pred(ex_2, P).
incorrect :- holds(s(P, obj_sensor), 36), is_visual_pred(P), not possible_pred(ex_3, P).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_0, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_0, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_2, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_0, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_3, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_0, P1),
	possible_pred(ex_2, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_0, P1),
	possible_pred(ex_3, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_2, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_3, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_2, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_2, P2),
	possible_pred(ex_2, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_2, P2),
	possible_pred(ex_3, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_3, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_3, P2),
	possible_pred(ex_2, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_2, P1),
	possible_pred(ex_3, P2),
	possible_pred(ex_3, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_2, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_0, P2),
	possible_pred(ex_3, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_2, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_2, P2),
	possible_pred(ex_2, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_2, P2),
	possible_pred(ex_3, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_3, P2),
	possible_pred(ex_0, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_3, P2),
	possible_pred(ex_2, P3).
incorrect :-
	holds(s(P1, obj_sensor), 34),
	holds(s(P2, obj_sensor), 35),
	holds(s(P3, obj_sensor), 36),
	possible_pred(ex_3, P1),
	possible_pred(ex_3, P2),
	possible_pred(ex_3, P3).
% unacceptable: (34): [False,False] (35): [False,False] (36): [False,False] 
% unacceptable: (34): [False,False] (35): [False,False] (36): [True,False] 
% unacceptable: (34): [False,False] (35): [False,False] (36): [True,True] 
% unacceptable: (34): [False,False] (35): [True,False] (36): [False,False] 
% unacceptable: (34): [False,False] (35): [True,True] (36): [False,False] 
% unacceptable: (34): [True,False] (35): [False,False] (36): [False,False] 
% unacceptable: (34): [True,False] (35): [False,False] (36): [True,False] 
% unacceptable: (34): [True,False] (35): [False,False] (36): [True,True] 
% unacceptable: (34): [True,False] (35): [True,False] (36): [False,False] 
% unacceptable: (34): [True,False] (35): [True,False] (36): [True,False] 
% unacceptable: (34): [True,False] (35): [True,False] (36): [True,True] 
% unacceptable: (34): [True,False] (35): [True,True] (36): [False,False] 
% unacceptable: (34): [True,False] (35): [True,True] (36): [True,False] 
% unacceptable: (34): [True,False] (35): [True,True] (36): [True,True] 
% unacceptable: (34): [True,True] (35): [False,False] (36): [False,False] 
% unacceptable: (34): [True,True] (35): [False,False] (36): [True,False] 
% unacceptable: (34): [True,True] (35): [False,False] (36): [True,True] 
% unacceptable: (34): [True,True] (35): [True,False] (36): [False,False] 
% unacceptable: (34): [True,True] (35): [True,False] (36): [True,False] 
% unacceptable: (34): [True,True] (35): [True,False] (36): [True,True] 
% unacceptable: (34): [True,True] (35): [True,True] (36): [False,False] 
% unacceptable: (34): [True,True] (35): [True,True] (36): [True,False] 
% unacceptable: (34): [True,True] (35): [True,True] (36): [True,True] 

% Elements
is_object(obj_sensor).
is_time(1..36).

% Concepts
is_concept(c_1).
is_concept(c_2).
is_concept(c_3).

% At most one
:- holds(s(c_1, X), T), holds(s(c_2, X), T).
:- holds(s(c_1, X), T), holds(s(c_3, X), T).
:- holds(s(c_2, X), T), holds(s(c_3, X), T).

% At least one
:-
	permanent(isa(t_sensor, X)),
	is_time(T),
	not holds(s(c_1, X), T),
	not holds(s(c_2, X), T),
	not holds(s(c_3, X), T).

% Incompossibility
incompossible(s(c_1, X), s(c_2, X)) :- permanent(isa(t_sensor, X)).
incompossible(s(c_1, X), s(c_3, X)) :- permanent(isa(t_sensor, X)).
incompossible(s(c_2, X), s(c_3, X)) :- permanent(isa(t_sensor, X)).

% Binary neural network
nodes(1, 2).
nodes(2, 3).
nodes(3, 3).

% Possible predicate assignments from bnn
possible_pred(BV, c_1) :- bnn_result(BV, 1, 1).
possible_pred(BV, c_2) :- bnn_result(BV, 2, 1).
possible_pred(BV, c_3) :- bnn_result(BV, 3, 1).

% Senses choice from bnn assignments
1 { senses(s(C, obj_sensor), T) : possible_pred(BV, C) } 1 :- bv_at(T, BV).


is_possible_pred(C) :- possible_pred(_, C).

count_bnn_examples_per_predicate(C, N) :-
	is_possible_pred(C),
	N = #count { E : possible_pred(E, C) }.

num_bvs(N) :- N = #count { BV : bv_at(T, BV), is_time(T) }.

max_bnn_examples_per_predicate(M) :-
	M = #max { N : count_bnn_examples_per_predicate(C, N) }.

:~ max_bnn_examples_per_predicate(M). [ M @ 2, M]
