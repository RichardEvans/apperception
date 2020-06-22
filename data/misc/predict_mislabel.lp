
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
%   11              off             on
%   12              on              off
%   13              on              on
%   14              off             on
%   15              on              off
%   16              on              on
%   17              off             on
%   18              on              off
%   19              on              on
%   20              off             on
%   21              ?               ?
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
senses(s(c_off, obj_sensor_a), 11).
senses(s(c_on, obj_sensor_b), 11).
senses(s(c_on, obj_sensor_a), 12).
senses(s(c_off, obj_sensor_b), 12).
senses(s(c_on, obj_sensor_a), 13).
senses(s(c_on, obj_sensor_b), 13).
senses(s(c_off, obj_sensor_a), 14).
senses(s(c_on, obj_sensor_b), 14).
senses(s(c_on, obj_sensor_a), 15).
senses(s(c_off, obj_sensor_b), 15).
senses(s(c_on, obj_sensor_a), 16).
senses(s(c_on, obj_sensor_b), 16).
senses(s(c_off, obj_sensor_a), 17).
senses(s(c_on, obj_sensor_b), 17).
senses(s(c_on, obj_sensor_a), 18).
senses(s(c_off, obj_sensor_b), 18).
senses(s(c_on, obj_sensor_a), 19).
senses(s(c_on, obj_sensor_b), 19).
senses(s(c_off, obj_sensor_a), 20).
senses(s(c_on, obj_sensor_b), 20).
senses(s(c_on, obj_sensor_a), 21).
senses(s(c_off, obj_sensor_b), 21).
senses(s(c_on, obj_sensor_a), 22).
senses(s(c_on, obj_sensor_b), 22).
senses(s(c_off, obj_sensor_a), 23).
senses(s(c_on, obj_sensor_b), 23).
senses(s(c_on, obj_sensor_a), 24).
senses(s(c_off, obj_sensor_b), 24).
senses(s(c_on, obj_sensor_a), 25).
senses(s(c_on, obj_sensor_b), 25).
senses(s(c_off, obj_sensor_a), 26).
senses(s(c_on, obj_sensor_b), 26).
senses(s(c_on, obj_sensor_a), 27).
senses(s(c_off, obj_sensor_b), 27).
senses(s(c_on, obj_sensor_a), 28).
senses(s(c_on, obj_sensor_b), 28).
senses(s(c_off, obj_sensor_a), 29).
senses(s(c_on, obj_sensor_b), 29).
hidden(s(c_on, obj_sensor_a), 30).
hidden(s(c_off, obj_sensor_b), 30).




% Elements
is_object(obj_sensor_a).
is_object(obj_sensor_b).
is_concept(c_on).
is_concept(c_off).
is_time(1..30).

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
