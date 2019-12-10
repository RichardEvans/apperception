
%* 
nodes(L, N) : there are N nodes at level L

A node is a functional term node(L, I), where 
    L is the layer
    I is the index
If I = 0, the node is a bias node.

is_bias(N) : N is a bias node.

weight(N, N2, B) : the weight from N to N2 is B.

num_inputs(N, C) : node N has C inputs.

count_threshold(N, T) : the threshold num 1s for node N to activate is T.

bnn_input(E, N, B): in example E, input node N starts with value B.
value(E, N, B) : in example E, node N has value B.
*%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The complete network %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

is_layer(L) :- nodes(L, _).

is_output_layer(L) :-
    is_layer(L),
    not is_layer(L+1).

is_bias(node(L, 0)) :- is_layer(L).

is_node(node(L, I)) :-
    is_layer(L),
    not is_output_layer(L),
    nodes(L, N),
    I = 0 .. N.

is_node(node(L, I)) :-
    is_output_layer(L),
    nodes(L, N),
    I = 1 .. N.

has_input(N, N2) :- weight(N2, N, _).

count_inputs(node(L, N), node(L-1, 0), 1) :-
    has_input(node(L, N), node(L-1, 0)).

count_inputs(node(L, N), node(L-1, 0), 0) :-
    is_node(node(L, N)),
    is_node(node(L-1, 0)),
    not has_input(node(L, N), node(L-1, 0)).

count_inputs(Output, N, C) :-
    count_inputs(Output, N2, C),
    next_node(N2, N),
    not has_input(Output, N).

count_inputs(Output, N, C+1) :-
    count_inputs(Output, N2, C),
    next_node(N2, N),
    has_input(Output, N).

num_inputs(Output, C) :-
    count_inputs(Output, N, C),
    last_node(N).

%%%%%%%%%%%%%%%%%%%%%%
%% Choosing weights %%
%%%%%%%%%%%%%%%%%%%%%%

0 { weight(node(L, I), node(L+1, J), B) : binary(B) } 1 :-
    is_node(node(L, I)),
    is_node(node(L+1, J)),
    J > 0.

binary(0).
binary(1).

%%%%%%%%%%%%%%%%%%
%% Forward pass %%
%%%%%%%%%%%%%%%%%%

threshold_count(N, C/2) :-
    num_inputs(N, C),
    C\2 < C/2.

threshold_count(N, C/2+1) :-
    num_inputs(N, C),
    C\2 >= C/2.

example(E) :- bnn_input(E, _, _).

value(E, N, 1) :- example(E), is_bias(N).
value(E, N, B) :- bnn_input(E, N, B).

nxor(0, 0, 1).
nxor(0, 1, 0).
nxor(1, 0, 0).
nxor(1, 1, 1).

intermediate(E, N1, N2, B) :-
    weight(N1, N2, W),
    value(E, N1, B2),
    nxor(W, B2, B).

next_node(node(L, N), node(L, N+1)) :-
    is_node(node(L, N)),
    is_node(node(L, N+1)).

last_node(node(L, N)) :-
    is_node(node(L, N)),
    not is_node(node(L, N+1)).

count_1(E, node(L, N), node(L-1, 0), 1) :-
    example(E),
    is_node(node(L, N)),
    is_node(node(L-1, 0)),
    weight(node(L-1, 0), node(L, N), 1).

count_1(E, node(L, N), node(L-1, 0), 0) :-
    example(E),
    is_node(node(L, N)),
    is_node(node(L-1, 0)),
    not weight(node(L-1, 0), node(L, N), 1).

count_1(E, Output, N, C) :-
    count_1(E, Output, N2, C),
    next_node(N2, N),
    not intermediate(E, N, Output, 1).

count_1(E, Output, N, C+1) :-
    count_1(E, Output, N2, C),
    next_node(N2, N),
    intermediate(E, N, Output, 1).
        
is_count_1(E, Output, C) :- count_1(E, Output, _, C).

count_1s(E, N, C) :-
    count_1(E, N, N2, C),
    last_node(N2).

value(E, N, 1) :-
    count_1s(E, N, C),
    threshold_count(N, T),
    C >= T.

value(E, N, 0) :-
    count_1s(E, N, C),
    threshold_count(N, T),
    C < T.
    
bnn_result(E, I, B) :- 
    is_output_layer(L),
    value(E, node(L, I), B).

% :~ weight(N, N2, B). [1@1, N, N2]
