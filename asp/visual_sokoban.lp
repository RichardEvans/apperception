:-  contains_visual_type(Cell, VT, T), 
    not some_type_at(Cell, VT, T), 
    VT != vt_type_0.

contains_visual_type(C, VT, T) :-
    sprite_at(T, C, E),
    sprite_type(E, VT).

is_test_time(T) :- 
    test_sprite_at(T, _, _).

nothing_at(T, C) :- 
    is_test_time(T), 
    is_cell(C), 
    not something_at(T, C).

possible_sprite_at(T, C, S) :- 
    is_test_time(T), 
    nothing_at(T, C), 
    sprite_type(S, vt_type_0).

:-  is_cell(C), 
    is_time(T), 
    not is_test_time(T), 
    not looks_something(C, T).

looks_something(C, T) :-  
    contains_visual_type(C, VT, T).
