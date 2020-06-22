
%------------------------------------------------------------------------------
% Definitions for the abstract letter objects.
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Typing
%------------------------------------------------------------------------------

is_given_concept(p_first_number).
is_given_concept(p_succ).
is_given_concept(p_less).
is_concept(p_first_number).
is_concept(p_succ).
is_concept(p_less).

permanent(isa(t_number, obj_number_0)).
permanent(isa(t_number, obj_number_1)).
permanent(isa(t_number, obj_number_2)).
permanent(isa(t_number, obj_number_3)).
permanent(isa(t_number, obj_number_4)).
permanent(isa(t_number, obj_number_5)).
permanent(isa(t_number, obj_number_6)).
permanent(isa(t_number, obj_number_7)).
permanent(isa(t_number, obj_number_8)).
permanent(isa(t_number, obj_number_9)).

permanent(isa(p_first_number, obj_number_0)).

%------------------------------------------------------------------------------
% Pure permanent facts
%------------------------------------------------------------------------------

permanent(isa2(p_succ, obj_number_0, obj_number_1)).
permanent(isa2(p_succ, obj_number_1, obj_number_2)).
permanent(isa2(p_succ, obj_number_2, obj_number_3)).
permanent(isa2(p_succ, obj_number_3, obj_number_4)).
permanent(isa2(p_succ, obj_number_4, obj_number_5)).
permanent(isa2(p_succ, obj_number_5, obj_number_6)).
permanent(isa2(p_succ, obj_number_6, obj_number_7)).
permanent(isa2(p_succ, obj_number_7, obj_number_8)).
permanent(isa2(p_succ, obj_number_8, obj_number_9)).

permanent(isa2(p_less, X, Y)) :- permanent(isa2(p_succ, X, Y)).

permanent(isa2(p_less, X, Y)) :- 
    permanent(isa2(p_succ, X, Z)),
    permanent(isa2(p_less, Z, Y)).

