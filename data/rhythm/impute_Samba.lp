% Tune generated from Samba

% Sensor readings
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_3), 1).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_3), 1).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 1).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_2), 2).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_2), 2).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 2).
hidden(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_1), 3).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_3), 3).
hidden(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_2), 4).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_1), 5).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 7).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_3), 7).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 7).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_2), 8).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_3), 9).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_3), 9).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 9).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_2), 10).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_2), 10).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 10).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_1), 11).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_3), 11).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 11).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 12).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_2), 12).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 12).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 13).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_1), 13).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 13).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 14).
hidden(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 14).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 14).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 15).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_3), 15).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 15).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 16).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_2), 16).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_0), 16).

% Elements
is_object(obj_sensor_bass_drum).
is_object(obj_sensor_snare_drum).
is_object(obj_sensor_hi_hat).

is_time(1..16).

% ∃! clause for c_loudness : at most one
:-
	holds(s2(c_loudness, X, Y), T),
	holds(s2(c_loudness, X, Y2), T),
	Y != Y2.

% ∃! clause for c_loudness : at least one
:-
	permanent(isa(t_sensor, X)),
	is_time(T),
	not aux_c_loudness(X, T).

aux_c_loudness(X, T) :-
	holds(s2(c_loudness, X, _), T).

% Incompossibility for p_r
incompossible(s2(c_loudness, X, Y), s2(c_loudness, X, Y2)) :-
	permanent(isa(t_sensor, X)),
	permanent(isa(t_loudness, Y)),
	permanent(isa(t_loudness, Y2)),
	Y != Y2.
