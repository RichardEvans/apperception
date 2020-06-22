
% Any two adjacent given atoms can either be simultaneous (in objective time)
% or successive (in objective time).
1 { sim((X,ST), (Y,ST+1)); succ((X,ST), (Y,ST+1)) } 1 :- given(X, ST), given(Y, ST+1).

% The position in objective time depends only on the sim/succ relations.
position((X,1), 1) :- given(X, 1).
position((X,ST), T) :- position((Y,ST2), T), sim((Y,ST2), (X,ST)).
position((X,ST), T+1) :- position((Y,ST2), T), succ((Y,ST2), (X,ST)), max_time(MT), T+1 <= MT.

% We project away the subjective time step to get the senses/2 predicate.
senses(X, T) :- position((X,_), T).

max_time(MT) :- is_st(MT), not is_st(MT+1).
is_st(ST) :- given(_, ST).

is_time(T) :- senses(_, T).
num_obj_time_steps(T) :- is_time(T), not is_time(T+1).

count_givens_per_objective_time(T, N) :-
    is_time(T),
    N = #count { G : position(G, T) }.

max_givens_per_objective_time(M) :-
    M = #max { N : count_givens_per_objective_time(T, N) }.

% We minimise the number of time-steps.
:~ num_obj_time_steps(T). [T @ 4]

% We do not allow two distinct readings from the same object in the same 
% moment of objective time. 
total_num_objects(N) :- N = #count { X : is_object(X) }.
:- max_givens_per_objective_time(N), total_num_objects(T), N > T.

% We provide the 1D spatial relation:
permanent(isa2(p_right, obj_cell_1, obj_cell_2)).
permanent(isa2(p_right, obj_cell_2, obj_cell_3)).
permanent(isa2(p_right, obj_cell_3, obj_cell_1)).