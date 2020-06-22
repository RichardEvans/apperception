
%------------------------------------------------------------------------------
% The sensory given.
%------------------------------------------------------------------------------

%--------------------------------------------------------
%   Time            Obj     Exogenous Action 
%                            
%   1               off     noop  
%   2               off     noop
%   3               off     noop
%   4               off     noop
%   5               off     noop
%   6               off     bong
%   7               on      noop
%   8               off     noop
%   9               off     noop
%   10              off     noop
%   11              off     bong
%   12              on      noop
%   13              off     bong
%   14              on      noop
%   15              off     noop
%   16              off     noop
%--------------------------------------------------------

% The given sequence
senses(s(c_off, obj_a), 1).
senses(s(c_off, obj_a), 2).
senses(s(c_off, obj_a), 3).
senses(s(c_off, obj_a), 4).
senses(s(c_off, obj_a), 5).
senses(s(c_off, obj_a), 6).
senses(s(c_on, obj_a), 7).
senses(s(c_off, obj_a), 8).
senses(s(c_off, obj_a), 9).
senses(s(c_off, obj_a), 10).
senses(s(c_off, obj_a), 11).
senses(s(c_on, obj_a), 12).
senses(s(c_off, obj_a), 13).
senses(s(c_on, obj_a), 14).
senses(s(c_off, obj_a), 15).
senses(s(c_off, obj_a), 16).

% Exogenous actions
exogenous(s(c_noop, obj_a), 1).
exogenous(s(c_noop, obj_a), 2).
exogenous(s(c_noop, obj_a), 3).
exogenous(s(c_noop, obj_a), 4).
exogenous(s(c_noop, obj_a), 5).
exogenous(s(c_bong, obj_a), 6).
exogenous(s(c_noop, obj_a), 7).
exogenous(s(c_noop, obj_a), 8).
exogenous(s(c_noop, obj_a), 9).
exogenous(s(c_noop, obj_a), 10).
exogenous(s(c_bong, obj_a), 11).
exogenous(s(c_noop, obj_a), 12).
exogenous(s(c_bong, obj_a), 13).
exogenous(s(c_noop, obj_a), 14).
exogenous(s(c_noop, obj_a), 15).
exogenous(s(c_noop, obj_a), 16).

% Elements
is_object(obj_a).
is_concept(c_on).
is_concept(c_off).
is_time(1..16).

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

% Input exclusions
% Every object is either bong or noop
% bong(X) ⊕ noop(X)

% At most one
:-
    holds(s(c_bong, X), T),
    holds(s(c_noop, X), T).

% At least one
:-
    permanent(isa(t_object, X)),
    is_time(T),
    not holds(s(c_bong, X), T),
    not holds(s(c_noop, X), T).

% Incompossibility
incompossible(s(c_bong, X), s(c_noop, X)) :-
    permanent(isa(t_object, X)).

exclusion_output("c_bong+c_noop").

