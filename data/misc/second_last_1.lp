

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
senses(s(c_off, obj_s), 1).
senses(s(c_off, obj_s), 2).
senses(s(c_on, obj_s), 3).
senses(s(c_off, obj_s), 4).
senses(s(c_on, obj_s), 5).
senses(s(c_on, obj_s), 6).
senses(s(c_on, obj_s), 7).
senses(s(c_off, obj_s), 8).
senses(s(c_off, obj_s), 9).
senses(s(c_on, obj_s), 10).
senses(s(c_off, obj_s), 11).
senses(s(c_on, obj_s), 12).
senses(s(c_off, obj_s), 13).
senses(s(c_on, obj_s), 14).
senses(s(c_off, obj_s), 15).
senses(s(c_off, obj_s), 16).


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
is_concept(c_on).
is_concept(c_off).
is_concept(c_value).
is_time(1..16).

% Input exclusions
% Every object is either on or off
% on(X) ⊕ off(X)
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

exclusion_output("c_on+c_off").

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

