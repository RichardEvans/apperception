part_of(Part, Whole, T) :-
    holds(s2(c_part_s, Part, Whole), T).

part_of(Part, Whole, T) :-
    is_time(T),
    permanent(isa2(p_part_c, Part, Whole)).

part_of(Part, Whole, T) :-
    holds(s2(c_part_c, Part, Whole), T).

