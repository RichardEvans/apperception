
%------------------------------------------------------------------------------
% Definitions for the number objects.
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Typing
%------------------------------------------------------------------------------

is_given_concept(p_is_0).
is_given_concept(p_is_1).
is_given_concept(p_is_2).
is_given_concept(p_is_3).
is_given_concept(p_num_succ).

is_concept(X) :- is_given_concept(X).

permanent(isa(t_num, obj_num_0)).
permanent(isa(t_num, obj_num_1)).
permanent(isa(t_num, obj_num_2)).
permanent(isa(t_num, obj_num_3)).

%------------------------------------------------------------------------------
% Pure permanent facts
%------------------------------------------------------------------------------

permanent(isa(p_is_0, obj_num_0)).
permanent(isa(p_is_1, obj_num_1)).
permanent(isa(p_is_2, obj_num_2)).
permanent(isa(p_is_3, obj_num_3)).

permanent(isa2(p_num_succ, obj_num_0, obj_num_1)).
permanent(isa2(p_num_succ, obj_num_1, obj_num_2)).
permanent(isa2(p_num_succ, obj_num_2, obj_num_3)).
