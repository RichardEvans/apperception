
%------------------------------------------------------------------------------
% The sensory given.
%------------------------------------------------------------------------------

%----------------------------------------
%   Time            Sensor a    Sensor b
%                            
%   1               ?           ?
%   2               off         a
%   3               on          a
%   4               off         ?
%   5               ?           ?
%   6               off         a
%   7               ?           ?
%----------------------------------------

% The given sequence
senses(s(c_off, obj_sensor_a), 2).
senses(s(c_a, obj_sensor_b), 2).
senses(s(c_on, obj_sensor_a), 3).
senses(s(c_a, obj_sensor_b), 3).
senses(s(c_off, obj_sensor_a), 4).
hidden(s(c_a, obj_sensor_b), 4).
senses(s(c_off, obj_sensor_a), 6).
senses(s(c_a, obj_sensor_b), 6).

% Elements
is_object(obj_sensor_a).
is_object(obj_sensor_b).
is_time(1..7).

% Input exclusions
% Every sensor (of type 1) is either on or off
% X : sensor_1 → on(X) ⊕ off(X)
% At most one
:-
    holds(s(c_on, X), T),
    holds(s(c_off, X), T).

% At least one
:-
    permanent(isa(t_sensor_1, X)),
    is_time(T),
    not holds(s(c_on, X), T),
    not holds(s(c_off, X), T).

% Incompossibility
incompossible(s(c_on, X), s(c_off, X)) :-
    permanent(isa(t_sensor_1, X)).

% Input exclusions
% Every sensor (of type 2) is either a, b, or c
% X : sensor_2 → a(X) ⊕ b(X) ⊕ c(X)

% At most one
:-
    holds(s(c_a, X), T),
    holds(s(c_b, X), T).

:-
    holds(s(c_a, X), T),
    holds(s(c_c, X), T).

:-
    holds(s(c_b, X), T),
    holds(s(c_c, X), T).

% At least one
:-
    permanent(isa(t_sensor_2, X)),
    is_time(T),
    not holds(s(c_a, X), T),
    not holds(s(c_b, X), T),
    not holds(s(c_c, X), T).

% Incompossibility
incompossible(s(c_a, X), s(c_b, X)) :-
    permanent(isa(t_sensor_2, X)).

incompossible(s(c_a, X), s(c_c, X)) :-
    permanent(isa(t_sensor_2, X)).

incompossible(s(c_b, X), s(c_c, X)) :-
    permanent(isa(t_sensor_2, X)).

exclusion_output("c_on+c_off").
exclusion_output("c_a+c_b").
