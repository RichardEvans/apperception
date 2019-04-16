%------------------------------------------------------------------------------
% This file was generated using rule 245
% with configuration ⬛⬛⬜
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% The sensory given
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Time  State
%
%   1	⬛⬛⬜
%   2	⬜⬛⬛
%   3	⬛⬜⬛
%   4	⬛⬛⬜
%   5	⬜⬛⬛
%   6	⬛⬜⬛
%------------------------------------------------------------------------------

% The given sequence
senses(s(c_on, obj_cell_1), 1).
senses(s(c_on, obj_cell_2), 1).
senses(s(c_off, obj_cell_1), 2).
senses(s(c_on, obj_cell_2), 2).
senses(s(c_on, obj_cell_1), 3).
senses(s(c_off, obj_cell_2), 3).
senses(s(c_on, obj_cell_1), 4).
senses(s(c_on, obj_cell_2), 4).
senses(s(c_off, obj_cell_1), 5).
senses(s(c_on, obj_cell_2), 5).
hidden(s(c_on, obj_cell_1), 6).
hidden(s(c_off, obj_cell_2), 6).

% Elements
is_object(obj_cell_1).
is_object(obj_cell_2).
is_time(1..6).
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
