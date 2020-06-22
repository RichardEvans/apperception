% No time abduction, discrete input.

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
senses(s(c_on, obj_cell_1), 6).
senses(s(c_off, obj_cell_2), 6).
senses(s(c_on, obj_cell_1), 7).
senses(s(c_on, obj_cell_2), 7).
senses(s(c_off, obj_cell_1), 8).
senses(s(c_on, obj_cell_2), 8).
senses(s(c_on, obj_cell_1), 9).
senses(s(c_off, obj_cell_2), 9).
senses(s(c_on, obj_cell_1), 10).
senses(s(c_on, obj_cell_2), 10).
senses(s(c_off, obj_cell_1), 11).
senses(s(c_on, obj_cell_2), 11).
senses(s(c_on, obj_cell_1), 12).
senses(s(c_off, obj_cell_2), 12).
hidden(s(c_on, obj_cell_1), 13).
hidden(s(c_on, obj_cell_2), 13).
hidden(s(c_off, obj_cell_1), 14).
hidden(s(c_on, obj_cell_2), 14).
hidden(s(c_on, obj_cell_1), 15).
hidden(s(c_off, obj_cell_2), 15).

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
