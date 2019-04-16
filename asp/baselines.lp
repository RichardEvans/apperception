

%------------------------------------------------------------------------------
% Constant baselines
%------------------------------------------------------------------------------

% k baseline for SW assumes everything is an 'a'
baseline_k_sw_incorrect :-
    is_object(obj_sensor),
    hidden(s2(c_letter, obj_sensor, L), _),
    L != obj_letter_a.

baseline_k_sw_correct :-
    is_object(obj_sensor),
    not baseline_k_sw_incorrect.

% k baseline for ECA assumes everything is off
baseline_k_eca_incorrect :-
    is_object(obj_cell_1),
    hidden(s(c_on, _), _).

baseline_k_eca_correct :-
    is_object(obj_cell_1),
    not baseline_k_eca_incorrect.

% k baseline for Music assumes loudness is 0
baseline_k_music_incorrect :-
    is_object(obj_sensor_c),
    hidden(s2(c_loudness, _, L), _),
    L != obj_loudness_0.

baseline_k_music_correct :-
    is_object(obj_sensor_c),
    not baseline_k_music_incorrect.

% k baseline for Rhythm assumes loudness is 0
baseline_k_rhythm_incorrect :-
    is_object(obj_sensor_bass_drum),
    hidden(s2(c_loudness, _, L), _),
    L != obj_loudness_0.

baseline_k_rhythm_correct :-
    is_object(obj_sensor_bass_drum),
    not baseline_k_rhythm_incorrect.

%------------------------------------------------------------------------------
% Inertia baselines
%------------------------------------------------------------------------------

baseline_inertia_sw_incorrect :-
    is_object(obj_sensor),
    senses(s2(c_letter, X, L), T),
    hidden(s2(c_letter, X, L2), T+1),
    L != L2.

baseline_inertia_sw_correct :- 
    is_object(obj_sensor),
    not baseline_inertia_sw_incorrect.

baseline_inertia_eca_incorrect :-
    is_object(obj_cell_1),
    senses(s(c_off, X), T),
    hidden(s(c_on, X), T+1).

baseline_inertia_eca_incorrect :-
    is_object(obj_cell_1),
    senses(s(c_on, X), T),
    hidden(s(c_off, X), T+1).

baseline_inertia_eca_correct :-
    is_object(obj_cell_1),
    not baseline_inertia_eca_incorrect.

baseline_inertia_music_incorrect :-
    is_object(obj_sensor_c),
    senses(s2(c_loudness, X, L), T),
    hidden(s2(c_loudness, X, L2), T+1),
    L != L2.

baseline_inertia_music_correct :-
    is_object(obj_sensor_c),
    not baseline_inertia_music_incorrect.

baseline_inertia_rhythm_incorrect :-
    is_object(obj_sensor_bass_drum),
    senses(s2(c_loudness, X, L), T),
    hidden(s2(c_loudness, X, L2), T+1),
    L != L2.

baseline_inertia_rhythm_correct :-
    is_object(obj_sensor_bass_drum),
    not baseline_inertia_rhythm_incorrect.

