% Tune generated from Minimal

% Sensor readings
senses(s2(c_loudness, obj_sensor_c, obj_loudness_0), 1).
senses(s2(c_loudness, obj_sensor_d, obj_loudness_0), 1).
senses(s2(c_loudness, obj_sensor_e, obj_loudness_0), 1).
senses(s2(c_loudness, obj_sensor_f, obj_loudness_0), 1).
senses(s2(c_loudness, obj_sensor_g, obj_loudness_2), 1).
senses(s2(c_loudness, obj_sensor_a, obj_loudness_0), 1).
senses(s2(c_loudness, obj_sensor_b, obj_loudness_0), 1).
senses(s2(c_loudness, obj_sensor_high_c, obj_loudness_0), 1).
hidden(s2(c_loudness, obj_sensor_c, obj_loudness_0), 2).
senses(s2(c_loudness, obj_sensor_d, obj_loudness_0), 2).
senses(s2(c_loudness, obj_sensor_e, obj_loudness_0), 2).
hidden(s2(c_loudness, obj_sensor_f, obj_loudness_0), 2).
senses(s2(c_loudness, obj_sensor_g, obj_loudness_1), 2).
senses(s2(c_loudness, obj_sensor_a, obj_loudness_0), 2).
senses(s2(c_loudness, obj_sensor_b, obj_loudness_0), 2).
senses(s2(c_loudness, obj_sensor_high_c, obj_loudness_0), 2).
hidden(s2(c_loudness, obj_sensor_c, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_d, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_e, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_f, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_g, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_a, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_b, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_high_c, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_c, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_d, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_e, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_f, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_g, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_a, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_b, obj_loudness_0), 4).
hidden(s2(c_loudness, obj_sensor_high_c, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_c, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_d, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_e, obj_loudness_2), 5).
senses(s2(c_loudness, obj_sensor_f, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_g, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_a, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_b, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_high_c, obj_loudness_0), 5).
senses(s2(c_loudness, obj_sensor_c, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_d, obj_loudness_0), 6).
hidden(s2(c_loudness, obj_sensor_e, obj_loudness_1), 6).
senses(s2(c_loudness, obj_sensor_f, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_g, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_a, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_b, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_high_c, obj_loudness_0), 6).
senses(s2(c_loudness, obj_sensor_c, obj_loudness_2), 7).
senses(s2(c_loudness, obj_sensor_d, obj_loudness_0), 7).
senses(s2(c_loudness, obj_sensor_e, obj_loudness_0), 7).
senses(s2(c_loudness, obj_sensor_f, obj_loudness_0), 7).
senses(s2(c_loudness, obj_sensor_g, obj_loudness_0), 7).
senses(s2(c_loudness, obj_sensor_a, obj_loudness_0), 7).
hidden(s2(c_loudness, obj_sensor_b, obj_loudness_0), 7).
hidden(s2(c_loudness, obj_sensor_high_c, obj_loudness_0), 7).
senses(s2(c_loudness, obj_sensor_c, obj_loudness_1), 8).
senses(s2(c_loudness, obj_sensor_d, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_e, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_f, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_g, obj_loudness_0), 8).
hidden(s2(c_loudness, obj_sensor_a, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_b, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_high_c, obj_loudness_0), 8).

% Elements
is_object(obj_sensor_c).
is_object(obj_sensor_d).
is_object(obj_sensor_e).
is_object(obj_sensor_f).
is_object(obj_sensor_g).
is_object(obj_sensor_a).
is_object(obj_sensor_b).
is_object(obj_sensor_high_c).

is_time(1..8).

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
