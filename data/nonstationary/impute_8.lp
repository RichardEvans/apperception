%------------------------------------------------------------------------------
% This file was generated using sequence ffffffedcbaaaaaa
%------------------------------------------------------------------------------

% The given sequence
senses(s2(c_letter, obj_sensor, obj_letter_f), 1).
senses(s2(c_letter, obj_sensor, obj_letter_f), 2).
hidden(s2(c_letter, obj_sensor, obj_letter_f), 3).
senses(s2(c_letter, obj_sensor, obj_letter_f), 4).
senses(s2(c_letter, obj_sensor, obj_letter_f), 5).
senses(s2(c_letter, obj_sensor, obj_letter_f), 6).
senses(s2(c_letter, obj_sensor, obj_letter_e), 7).
senses(s2(c_letter, obj_sensor, obj_letter_d), 8).
senses(s2(c_letter, obj_sensor, obj_letter_c), 9).
senses(s2(c_letter, obj_sensor, obj_letter_b), 10).
senses(s2(c_letter, obj_sensor, obj_letter_a), 11).
senses(s2(c_letter, obj_sensor, obj_letter_a), 12).
senses(s2(c_letter, obj_sensor, obj_letter_a), 13).
senses(s2(c_letter, obj_sensor, obj_letter_a), 14).
senses(s2(c_letter, obj_sensor, obj_letter_a), 15).
senses(s2(c_letter, obj_sensor, obj_letter_a), 16).

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
