% Time abduction, discrete input.


% The given sequence
given(s(c_on, obj_cell_2), 1).
given(s(c_on, obj_cell_1), 2).
given(s(c_off, obj_cell_1), 3).
given(s(c_on, obj_cell_2), 4).
given(s(c_off, obj_cell_2), 5).
given(s(c_on, obj_cell_1), 6).
given(s(c_on, obj_cell_1), 7).
given(s(c_on, obj_cell_2), 8).
given(s(c_on, obj_cell_2), 9).
given(s(c_off, obj_cell_1), 10).
given(s(c_on, obj_cell_1), 11).
given(s(c_off, obj_cell_2), 12).
given(s(c_on, obj_cell_2), 13).
given(s(c_on, obj_cell_1), 14).
given(s(c_off, obj_cell_1), 15).
given(s(c_on, obj_cell_2), 16).
given(s(c_off, obj_cell_2), 17).
given(s(c_on, obj_cell_1), 18).
given(s(c_on, obj_cell_1), 19).
given(s(c_on, obj_cell_2), 20).
given(s(c_on, obj_cell_2), 21).
given(s(c_off, obj_cell_1), 22).
given(s(c_on, obj_cell_1), 23).
given(s(c_off, obj_cell_2), 24).
given(s(c_on, obj_cell_2), 25).
given(s(c_on, obj_cell_1), 26).
given(s(c_off, obj_cell_1), 27).
given(s(c_on, obj_cell_2), 28).
given(s(c_off, obj_cell_2), 29).
given(s(c_on, obj_cell_1), 30).

% Elements
is_object(obj_cell_1).
is_object(obj_cell_2).
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

