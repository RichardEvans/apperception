
%------------------------------------------------------------------------------
% The sensory given.
%------------------------------------------------------------------------------

%----------------------------
%   Time            Sensor a 
%                            
%   1               on       
%   2               off      
%   3               on       
%   4               off      
%   5               on       
%   6               off      
%----------------------------

% The given sequence
senses(s(c_on, obj_sensor_a), 1).
senses(s(c_off, obj_sensor_a), 2).
senses(s(c_on, obj_sensor_a), 3).
senses(s(c_off, obj_sensor_a), 4).
senses(s(c_on, obj_sensor_a), 5).
hidden(s(c_off, obj_sensor_a), 6).

% Elements
is_object(obj_sensor_a).
is_concept(c_on).
is_concept(c_off).
is_time(1..6).

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
