
%------------------------------------------------------------------------------
% Definitions for the touch objects.
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Typing
%------------------------------------------------------------------------------

is_given_concept(p_min_touch).
is_given_concept(p_max_touch).
is_given_concept(p_succ).
is_concept(p_min_touch).
is_concept(p_max_touch).
is_concept(p_succ).

permanent(isa(t_touch, obj_touch_0)).
permanent(isa(t_touch, obj_touch_1)).
permanent(isa(t_touch, obj_touch_2)).
permanent(isa(t_touch, obj_touch_3)).

permanent(isa(p_min_touch, obj_touch_0)).
permanent(isa(p_max_touch, obj_touch_3)).

%------------------------------------------------------------------------------
% Pure permanent facts
%------------------------------------------------------------------------------

permanent(isa2(p_succ, obj_touch_0, obj_touch_1)).
permanent(isa2(p_succ, obj_touch_1, obj_touch_2)).
permanent(isa2(p_succ, obj_touch_2, obj_touch_3)).

