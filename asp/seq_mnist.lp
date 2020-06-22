

% Choose how to interpret low-level input.
1 { senses(s2(c_value, obj_sensor, N), T) : possible_senses(s2(c_value, obj_sensor, N), T) } 1 :- is_time(T), not hidden(_, T).



% Elements
is_object(obj_sensor).

is_time(T) :- possible_senses(_, T).
is_time(T) :- hidden(_, T).

% Every sensor has a unique value attribute

% ∃! clause for c_value : at most one

:-
    holds(s2(c_value, X, Y), T), 
    holds(s2(c_value, X, Y2), T), 
    Y != Y2.

% ∃! clause for c_value : at least one

:-
    permanent(isa(t_sensor, X)),
    is_time(T),
    not aux_c_value(X, T).

aux_c_value(X, T) :-
    holds(s2(c_value, X, _), T).

% Incompossibility for value
incompossible(s2(c_value, X, Y), s2(c_value, X, Y2)) :-
    permanent(isa(t_sensor, X)),
    permanent(isa(t_number, Y)),
    permanent(isa(t_number, Y2)),
    Y != Y2.
