
%------------------------------------------------------------------------------
% The sensory given.
%------------------------------------------------------------------------------

%---------------------------------------------
%   Time            Sensor a        Sensor b
%                            
%   1               ?               ?
%   2               off             on
%   3               on              off
%   4               on              on
%   5               ?               on
%   6               on              off  
%   7               on              on
%   8               off             on
%   9               on              ?
%   10              ?               ?
%---------------------------------------------

% The given sequence
hidden(s(c_on, obj_sensor_a), 1).
hidden(s(c_on, obj_sensor_b), 1).
senses(s(c_off, obj_sensor_a), 2).
senses(s(c_on, obj_sensor_b), 2).
senses(s(c_on, obj_sensor_a), 3).
senses(s(c_off, obj_sensor_b), 3).
senses(s(c_on, obj_sensor_a), 4).
senses(s(c_on, obj_sensor_b), 4).
hidden(s(c_off, obj_sensor_a), 5).
senses(s(c_on, obj_sensor_b), 5).
senses(s(c_on, obj_sensor_a), 6).
senses(s(c_off, obj_sensor_b), 6).
senses(s(c_on, obj_sensor_a), 7).
senses(s(c_on, obj_sensor_b), 7).
senses(s(c_off, obj_sensor_a), 8).
senses(s(c_on, obj_sensor_b), 8).
senses(s(c_on, obj_sensor_a), 9).
hidden(s(c_off, obj_sensor_b), 9).
hidden(s(c_on, obj_sensor_a), 10).
hidden(s(c_on, obj_sensor_b), 10).


% Elements
is_object(obj_sensor_a).
is_object(obj_sensor_b).
is_concept(c_on).
is_concept(c_off).
is_time(1..10).

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
    permanent(isa(t_object, X)),
    is_time(T),
    not holds(s(c_on, X), T),
    not holds(s(c_off, X), T).

% Incompossibility
incompossible(s(c_on, X), s(c_off, X)) :-
    permanent(isa(t_object, X)).

exclusion_output("c_on+c_off").
