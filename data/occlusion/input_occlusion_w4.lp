%--------------------------------------------------
% Auto-generated from Occlusion.hs
%
% Time 1:
% 1....
% 2....
% 3....
%
% Time 2:
% 1....
% 2....
% ....3
%
% Time 3:
% 1....
% .2...
% ...3.
%
% Time 4:
% ....1
% .2...
% ..3..
%
% Time 5:
% ....1
% ..2..
% .3...
%
% Time 6:
% ....1
% ..2..
% 3....
%
% Time 7:
% ...1.
% ...2.
% ....3
%
% Time 8:
% ...1.
% ...2.
% ...3.
%
% Time 9:
% ...1.
% ....2
% ..3..
%
% Time 10:
% ..1..
% ....2
% .3...
%
%--------------------------------------------------

hidden(s2(c_in, obj_m1, obj_c1), 1).
hidden(s2(c_in, obj_m2, obj_c6), 1).
senses(s2(c_in, obj_m3, obj_c11), 1).
hidden(s2(c_in, obj_m1, obj_c1), 2).
senses(s2(c_in, obj_m2, obj_c6), 2).
senses(s2(c_in, obj_m3, obj_c15), 2).
senses(s2(c_in, obj_m1, obj_c1), 3).
senses(s2(c_in, obj_m2, obj_c7), 3).
senses(s2(c_in, obj_m3, obj_c14), 3).
senses(s2(c_in, obj_m1, obj_c5), 4).
senses(s2(c_in, obj_m2, obj_c7), 4).
senses(s2(c_in, obj_m3, obj_c13), 4).
senses(s2(c_in, obj_m1, obj_c5), 5).
senses(s2(c_in, obj_m2, obj_c8), 5).
senses(s2(c_in, obj_m3, obj_c12), 5).
senses(s2(c_in, obj_m1, obj_c5), 6).
senses(s2(c_in, obj_m2, obj_c8), 6).
senses(s2(c_in, obj_m3, obj_c11), 6).
hidden(s2(c_in, obj_m1, obj_c4), 7).
senses(s2(c_in, obj_m2, obj_c9), 7).
senses(s2(c_in, obj_m3, obj_c15), 7).
hidden(s2(c_in, obj_m1, obj_c4), 8).
hidden(s2(c_in, obj_m2, obj_c9), 8).
senses(s2(c_in, obj_m3, obj_c14), 8).
senses(s2(c_in, obj_m1, obj_c4), 9).
senses(s2(c_in, obj_m2, obj_c10), 9).
senses(s2(c_in, obj_m3, obj_c13), 9).
senses(s2(c_in, obj_m1, obj_c3), 10).
senses(s2(c_in, obj_m2, obj_c10), 10).
senses(s2(c_in, obj_m3, obj_c12), 10).

% Elements
is_object(obj_c1).
is_object(obj_c2).
is_object(obj_c3).
is_object(obj_c4).
is_object(obj_c5).
is_object(obj_c6).
is_object(obj_c7).
is_object(obj_c8).
is_object(obj_c9).
is_object(obj_c10).
is_object(obj_c11).
is_object(obj_c12).
is_object(obj_c13).
is_object(obj_c14).
is_object(obj_c15).
is_object(obj_m1).
is_object(obj_m2).
is_object(obj_m3).
is_time(1..10).

is_concept(c_in).

% ∃! clause for c_in : at most one
:-
   holds(s2(c_in, X, Y), T),
   holds(s2(c_in, X, Y2), T),
   Y != Y2.

% ∃! clause for c_in : at least one
:-
   permanent(isa(t_mover, X)),
   is_time(T),
   not aux_c_in(X, T).

aux_c_in(X, T) :-
   holds(s2(c_in, X, _), T).

% Incompossibility for in
incompossible(s2(c_in, X, Y), s2(c_in, X, Y2)) :-
   permanent(isa(t_mover, X)),
   permanent(isa(t_cell, Y)),
   permanent(isa(t_cell, Y2)),
   Y != Y2.

permanent(isa2(p_right, obj_c1, obj_c2)).
permanent(isa2(p_right, obj_c2, obj_c3)).
permanent(isa2(p_right, obj_c3, obj_c4)).
permanent(isa2(p_right, obj_c4, obj_c5)).
permanent(isa2(p_right, obj_c5, obj_c1)).
permanent(isa2(p_right, obj_c6, obj_c7)).
permanent(isa2(p_right, obj_c7, obj_c8)).
permanent(isa2(p_right, obj_c8, obj_c9)).
permanent(isa2(p_right, obj_c9, obj_c10)).
permanent(isa2(p_right, obj_c10, obj_c6)).
permanent(isa2(p_right, obj_c11, obj_c12)).
permanent(isa2(p_right, obj_c12, obj_c13)).
permanent(isa2(p_right, obj_c13, obj_c14)).
permanent(isa2(p_right, obj_c14, obj_c15)).
permanent(isa2(p_right, obj_c15, obj_c11)).
permanent(isa2(p_below, obj_c1, obj_c4)).
permanent(isa2(p_below, obj_c6, obj_c9)).
permanent(isa2(p_below, obj_c2, obj_c5)).
permanent(isa2(p_below, obj_c7, obj_c10)).
permanent(isa2(p_below, obj_c3, obj_c6)).
permanent(isa2(p_below, obj_c8, obj_c11)).
permanent(isa2(p_below, obj_c4, obj_c7)).
permanent(isa2(p_below, obj_c9, obj_c12)).
permanent(isa2(p_below, obj_c5, obj_c8)).
permanent(isa2(p_below, obj_c10, obj_c13)).
