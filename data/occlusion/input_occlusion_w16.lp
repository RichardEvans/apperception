%--------------------------------------------------
% Auto-generated from Occlusion.hs
%
% Time 1:
% 1......
% .2.....
% ..3....
% ...4...
%
% Time 2:
% 1......
% .2.....
% .3.....
% ..4....
%
% Time 3:
% 1......
% 2......
% 3......
% .4.....
%
% Time 4:
% ......1
% 2......
% ......3
% 4......
%
% Time 5:
% ......1
% ......2
% .....3.
% ......4
%
% Time 6:
% ......1
% ......2
% ....3..
% .....4.
%
% Time 7:
% .....1.
% .....2.
% ...3...
% ....4..
%
% Time 8:
% .....1.
% .....2.
% ..3....
% ...4...
%
% Time 9:
% .....1.
% ....2..
% .3.....
% ..4....
%
% Time 10:
% ....1..
% ....2..
% 3......
% .4.....
%
%--------------------------------------------------

senses(s2(c_in, obj_m1, obj_c1), 1).
senses(s2(c_in, obj_m2, obj_c9), 1).
senses(s2(c_in, obj_m3, obj_c17), 1).
senses(s2(c_in, obj_m4, obj_c25), 1).
senses(s2(c_in, obj_m1, obj_c1), 2).
hidden(s2(c_in, obj_m2, obj_c9), 2).
senses(s2(c_in, obj_m3, obj_c16), 2).
senses(s2(c_in, obj_m4, obj_c24), 2).
hidden(s2(c_in, obj_m1, obj_c1), 3).
hidden(s2(c_in, obj_m2, obj_c8), 3).
senses(s2(c_in, obj_m3, obj_c15), 3).
senses(s2(c_in, obj_m4, obj_c23), 3).
hidden(s2(c_in, obj_m1, obj_c7), 4).
hidden(s2(c_in, obj_m2, obj_c8), 4).
senses(s2(c_in, obj_m3, obj_c21), 4).
senses(s2(c_in, obj_m4, obj_c22), 4).
hidden(s2(c_in, obj_m1, obj_c7), 5).
hidden(s2(c_in, obj_m2, obj_c14), 5).
senses(s2(c_in, obj_m3, obj_c20), 5).
senses(s2(c_in, obj_m4, obj_c28), 5).
hidden(s2(c_in, obj_m1, obj_c7), 6).
senses(s2(c_in, obj_m2, obj_c14), 6).
senses(s2(c_in, obj_m3, obj_c19), 6).
senses(s2(c_in, obj_m4, obj_c27), 6).
hidden(s2(c_in, obj_m1, obj_c6), 7).
senses(s2(c_in, obj_m2, obj_c13), 7).
senses(s2(c_in, obj_m3, obj_c18), 7).
senses(s2(c_in, obj_m4, obj_c26), 7).
hidden(s2(c_in, obj_m1, obj_c6), 8).
senses(s2(c_in, obj_m2, obj_c13), 8).
senses(s2(c_in, obj_m3, obj_c17), 8).
senses(s2(c_in, obj_m4, obj_c25), 8).
senses(s2(c_in, obj_m1, obj_c6), 9).
senses(s2(c_in, obj_m2, obj_c12), 9).
senses(s2(c_in, obj_m3, obj_c16), 9).
senses(s2(c_in, obj_m4, obj_c24), 9).
hidden(s2(c_in, obj_m1, obj_c5), 10).
senses(s2(c_in, obj_m2, obj_c12), 10).
senses(s2(c_in, obj_m3, obj_c15), 10).
senses(s2(c_in, obj_m4, obj_c23), 10).

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
is_object(obj_c16).
is_object(obj_c17).
is_object(obj_c18).
is_object(obj_c19).
is_object(obj_c20).
is_object(obj_c21).
is_object(obj_c22).
is_object(obj_c23).
is_object(obj_c24).
is_object(obj_c25).
is_object(obj_c26).
is_object(obj_c27).
is_object(obj_c28).
is_object(obj_m1).
is_object(obj_m2).
is_object(obj_m3).
is_object(obj_m4).
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
permanent(isa2(p_right, obj_c5, obj_c6)).
permanent(isa2(p_right, obj_c6, obj_c7)).
permanent(isa2(p_right, obj_c7, obj_c1)).
permanent(isa2(p_right, obj_c8, obj_c9)).
permanent(isa2(p_right, obj_c9, obj_c10)).
permanent(isa2(p_right, obj_c10, obj_c11)).
permanent(isa2(p_right, obj_c11, obj_c12)).
permanent(isa2(p_right, obj_c12, obj_c13)).
permanent(isa2(p_right, obj_c13, obj_c14)).
permanent(isa2(p_right, obj_c14, obj_c8)).
permanent(isa2(p_right, obj_c15, obj_c16)).
permanent(isa2(p_right, obj_c16, obj_c17)).
permanent(isa2(p_right, obj_c17, obj_c18)).
permanent(isa2(p_right, obj_c18, obj_c19)).
permanent(isa2(p_right, obj_c19, obj_c20)).
permanent(isa2(p_right, obj_c20, obj_c21)).
permanent(isa2(p_right, obj_c21, obj_c15)).
permanent(isa2(p_right, obj_c22, obj_c23)).
permanent(isa2(p_right, obj_c23, obj_c24)).
permanent(isa2(p_right, obj_c24, obj_c25)).
permanent(isa2(p_right, obj_c25, obj_c26)).
permanent(isa2(p_right, obj_c26, obj_c27)).
permanent(isa2(p_right, obj_c27, obj_c28)).
permanent(isa2(p_right, obj_c28, obj_c22)).
permanent(isa2(p_below, obj_c1, obj_c4)).
permanent(isa2(p_below, obj_c8, obj_c11)).
permanent(isa2(p_below, obj_c15, obj_c18)).
permanent(isa2(p_below, obj_c2, obj_c5)).
permanent(isa2(p_below, obj_c9, obj_c12)).
permanent(isa2(p_below, obj_c16, obj_c19)).
permanent(isa2(p_below, obj_c3, obj_c6)).
permanent(isa2(p_below, obj_c10, obj_c13)).
permanent(isa2(p_below, obj_c17, obj_c20)).
permanent(isa2(p_below, obj_c4, obj_c7)).
permanent(isa2(p_below, obj_c11, obj_c14)).
permanent(isa2(p_below, obj_c18, obj_c21)).
permanent(isa2(p_below, obj_c5, obj_c8)).
permanent(isa2(p_below, obj_c12, obj_c15)).
permanent(isa2(p_below, obj_c19, obj_c22)).
permanent(isa2(p_below, obj_c6, obj_c9)).
permanent(isa2(p_below, obj_c13, obj_c16)).
permanent(isa2(p_below, obj_c20, obj_c23)).
permanent(isa2(p_below, obj_c7, obj_c10)).
permanent(isa2(p_below, obj_c14, obj_c17)).
permanent(isa2(p_below, obj_c21, obj_c24)).
