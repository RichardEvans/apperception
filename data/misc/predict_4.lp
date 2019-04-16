%------------------------------------------------------------------------------
% This is the input for learning the p_succ permanent relation.
%------------------------------------------------------------------------------

% The given sequence
senses(s2(c_letter, obj_sensor, obj_letter_a), 1).
senses(s2(c_letter, obj_sensor, obj_letter_b), 2).
senses(s2(c_letter, obj_sensor, obj_letter_c), 3).
senses(s2(c_letter, obj_sensor, obj_letter_d), 4).
senses(s2(c_letter, obj_sensor, obj_letter_e), 5).
senses(s2(c_letter, obj_sensor, obj_letter_f), 6).
senses(s2(c_letter, obj_sensor, obj_letter_a), 7).
senses(s2(c_letter, obj_sensor, obj_letter_b), 8).
senses(s2(c_letter, obj_sensor, obj_letter_c), 9).
hidden(s2(c_letter, obj_sensor, obj_letter_d), 10).

% Elements
is_object(obj_sensor).
is_time(1..10).

% Every sensor has a unique letter attribute

% ∃! clause for c_letter : at most one

:-
	holds(s2(c_letter, X, Y), T), 
	holds(s2(c_letter, X, Y2), T), 
	Y != Y2.

% ∃! clause for c_letter : at least one

:-
	permanent(isa(t_sensor, X)),
	is_time(T),
	not aux_c_letter(X, T).

aux_c_letter(X, T) :-
	holds(s2(c_letter, X, _), T).

% Incompossibility for letter
incompossible(s2(c_letter, X, Y), s2(c_letter, X, Y2)) :-
	permanent(isa(t_sensor, X)),
	permanent(isa(t_letter, Y)),
	permanent(isa(t_letter, Y2)),
	Y != Y2.
