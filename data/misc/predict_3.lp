
%------------------------------------------------------------------------------
% The sensory given.
%------------------------------------------------------------------------------

%----------------------------
%   Time            Sensor a 
%                            
%   1               a       
%   2               b      
%   3               c       
%   4               d      
%   5               a       
%   6               b      
%   7               c       
%   8               d      
%   9               a       
%   10              b      
%   11              c       
%   12              ?      
%----------------------------

% The given sequence
senses(s(c_a, obj_sensor_a), 1).
senses(s(c_b, obj_sensor_a), 2).
senses(s(c_c, obj_sensor_a), 3).
senses(s(c_d, obj_sensor_a), 4).
senses(s(c_a, obj_sensor_a), 5).
senses(s(c_b, obj_sensor_a), 6).
senses(s(c_c, obj_sensor_a), 7).
senses(s(c_d, obj_sensor_a), 8).
senses(s(c_a, obj_sensor_a), 9).
senses(s(c_b, obj_sensor_a), 10).
senses(s(c_c, obj_sensor_a), 11).
hidden(s(c_d, obj_sensor_a), 12).

% Elements
is_object(obj_sensor_a).
is_time(1..12).

% Input exclusions
% Every object is either a or b or c or d
% a(X) ⊕ b(X) ⊕ c(X) ⊕ d(X)

% At most one
:-
    holds(s(c_a, X), T),
    holds(s(c_b, X), T).

:-
    holds(s(c_a, X), T),
    holds(s(c_c, X), T).

:-
    holds(s(c_a, X), T),
    holds(s(c_d, X), T).

:-
    holds(s(c_b, X), T),
    holds(s(c_c, X), T).

:-
    holds(s(c_b, X), T),
    holds(s(c_d, X), T).

:-
    holds(s(c_c, X), T),
    holds(s(c_d, X), T).

% At least one
:-
    permanent(isa(t_object, X)),
    is_time(T),
    not holds(s(c_a, X), T),
    not holds(s(c_b, X), T),
    not holds(s(c_c, X), T),
    not holds(s(c_d, X), T).

% Incompossibility
incompossible(s(c_a, X), s(c_b, X)) :-
    permanent(isa(t_object, X)).

incompossible(s(c_a, X), s(c_c, X)) :-
    permanent(isa(t_object, X)).

incompossible(s(c_a, X), s(c_d, X)) :-
    permanent(isa(t_object, X)).

incompossible(s(c_b, X), s(c_c, X)) :-
    permanent(isa(t_object, X)).

incompossible(s(c_b, X), s(c_d, X)) :-
    permanent(isa(t_object, X)).

incompossible(s(c_c, X), s(c_d, X)) :-
    permanent(isa(t_object, X)).

exclusion_output("c_a+c_b+c_c+c_d").
