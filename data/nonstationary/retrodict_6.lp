%------------------------------------------------------------------------------
% This file was generated using sequence fedcbaaaaaabcdef
%------------------------------------------------------------------------------

% The given sequence
hidden(s2(c_letter, obj_sensor, obj_letter_f), 1).
senses(s2(c_letter, obj_sensor, obj_letter_e), 2).
senses(s2(c_letter, obj_sensor, obj_letter_d), 3).
senses(s2(c_letter, obj_sensor, obj_letter_c), 4).
senses(s2(c_letter, obj_sensor, obj_letter_b), 5).
senses(s2(c_letter, obj_sensor, obj_letter_a), 6).
senses(s2(c_letter, obj_sensor, obj_letter_a), 7).
senses(s2(c_letter, obj_sensor, obj_letter_a), 8).
senses(s2(c_letter, obj_sensor, obj_letter_a), 9).
senses(s2(c_letter, obj_sensor, obj_letter_a), 10).
senses(s2(c_letter, obj_sensor, obj_letter_a), 11).
senses(s2(c_letter, obj_sensor, obj_letter_b), 12).
senses(s2(c_letter, obj_sensor, obj_letter_c), 13).
senses(s2(c_letter, obj_sensor, obj_letter_d), 14).
senses(s2(c_letter, obj_sensor, obj_letter_e), 15).
senses(s2(c_letter, obj_sensor, obj_letter_f), 16).

% Elements
is_object(obj_sensor).
is_time(1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16).

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
