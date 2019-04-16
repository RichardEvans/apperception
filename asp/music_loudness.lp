
%------------------------------------------------------------------------------
% Definitions for the loudness objects.
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Typing
%------------------------------------------------------------------------------

is_given_concept(p_min_loudness).
is_given_concept(p_max_loudness).
is_given_concept(p_succ).
is_concept(p_min_loudness).
is_concept(p_max_loudness).
is_concept(p_succ).

permanent(isa(t_loudness, obj_loudness_0)).
permanent(isa(t_loudness, obj_loudness_1)).
permanent(isa(t_loudness, obj_loudness_2)).

permanent(isa(p_min_loudness, obj_loudness_0)).
permanent(isa(p_max_loudness, obj_loudness_2)).

%------------------------------------------------------------------------------
% Pure permanent facts
%------------------------------------------------------------------------------

permanent(isa2(p_succ, obj_loudness_0, obj_loudness_1)).
permanent(isa2(p_succ, obj_loudness_1, obj_loudness_2)).

