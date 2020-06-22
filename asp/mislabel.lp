

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