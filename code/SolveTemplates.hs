module SolveTemplates where

import Interpretation
    
----------------------------- ECA templates -----------------------------------

make_eca_template :: Bool -> String -> Int -> (String, Template)
make_eca_template hard_code_space input_f n = (s, t) where
    s = "Using " ++ show n ++ " causes rules"
    t = (template_eca hard_code_space) { num_causes_rules = n }

frame_eca :: Bool -> Int -> Frame
frame_eca hard_code_space n = Frame {
    types = [T "sensor"],
    type_hierarchy = [],
    objects = [(O ("cell_" ++ show i), T "sensor") | i <- [1..n]],
    exogeneous_objects = [],
    permanent_concepts = [
        (P "r", if hard_code_space then Given else Constructed, [T "sensor", T "sensor"])
    ],
    fluid_concepts = [
        (C "on", [T "sensor"]), 
        (C "off", [T "sensor"])
        ],
    input_concepts = [C "on", C "off"],
    vars = [
        (V "s", T "sensor"),
        (V "s2", T "sensor"),
        (V "s3", T "sensor")
        ],
    var_groups = [
        [V "s", V "s2", V "s3"]
        ],
    aux_files = if hard_code_space then ["space11.lp"] else []
}

template_eca_n :: Bool -> Int -> Template
template_eca_n hard_code_space n = Template {
    dir = "eca",
    frame = frame_eca hard_code_space n,
    min_body_atoms = 1,
    max_body_atoms = 5, 
    num_arrow_rules = 0,
    num_causes_rules = 4
    }   

template_eca :: Bool -> Template
template_eca hard_code_space = template_eca_n hard_code_space 11

-- A minimal frame for ECA with just two sensors
frame_eca_small :: Frame
frame_eca_small = Frame {
    types = [T "sensor", T "grid", T "object"],
    type_hierarchy = [
        (T "object", [T "sensor", T "grid"])
        ],
    objects = [
        (O "cell_1", T "sensor"),
        (O "cell_2", T "sensor"),
        (O "grid", T "grid")
        ],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [
        (C "on", [T "sensor"]), 
        (C "off", [T "sensor"]), 
        (C "part", [T "sensor", T "grid"])
        ],
    input_concepts = [C "on", C "off"],
    vars = [
        (V "s", T "sensor"),
        (V "s2", T "sensor")
        ],
    var_groups = [
        [V "s"],
        [V "s", V "s2"]
        ],
    aux_files = []
}       

template_eca_small :: Template
template_eca_small = Template { 
    dir = "eca",
    frame = frame_eca_small,
    min_body_atoms = 0,
    max_body_atoms = 2, 
    num_arrow_rules = 1,
    num_causes_rules = 3
    }


-------------------------------------------------------------------------------
-- Music templates
-------------------------------------------------------------------------------

frame_music :: Frame
frame_music = Frame {
    types = [T "object", T "sensor", T "finger", T "loudness"],
    type_hierarchy = [
        (T "object", [T "finger", T "sensor"])
        ],
    objects = [
        (O "sensor_c", T "sensor"),
        (O "sensor_d", T "sensor"),
        (O "sensor_e", T "sensor"),
        (O "sensor_f", T "sensor"),
        (O "sensor_g", T "sensor"),
        (O "sensor_a", T "sensor"),
        (O "sensor_b", T "sensor"),
        (O "sensor_high_c", T "sensor"),
        (O "finger", T "finger"),
        (O "grid", T "grid"),
        (O "loudness_0", T "loudness"),
        (O "loudness_1", T "loudness"),
        (O "loudness_2", T "loudness")
        ],    
    exogeneous_objects = [O "loudness_0", O "loudness_1", O "loudness_2"],
    permanent_concepts = [
        (P "min_loudness", Given, [T "loudness"]),
        (P "max_loudness", Given, [T "loudness"]),
        (P "succ", Given, [T "loudness", T "loudness"]),
        (P "r", Given, [T "sensor", T "sensor"]),
        (P "arp", Given, [T "sensor", T "sensor"])
        ],
    fluid_concepts = [
        (C "loudness", [T "sensor", T "loudness"]), 
        (C "on", [T "finger", T "sensor"]),
        (C "s1", [T "sensor"]),
        (C "s2", [T "sensor"]),
        (C "p1", [T "finger"]),
        (C "p2", [T "finger"]),
        (C "p3", [T "finger"]),
        (C "p4", [T "finger"]),
        (C "p5", [T "finger"]),
        (C "p6", [T "finger"])
        ],
    input_concepts = [C "loudness"],
    vars = [
        (V "f",       T "finger"),
        (V "l",       T "loudness"),
        (V "l2",      T "loudness"),
        (V "s",       T "sensor"),
        (V "s2",      T "sensor")
        ],
    var_groups = [
        [V "l", V "s"],
        [V "f", V "s", V "s2"],
        [V "l", V "l2", V "s"]        
        ],
    aux_files = ["music_loudness.lp", "music_space.lp"]
}

template_music :: Template
template_music = Template {
    dir = "music",
    frame = frame_music,
    min_body_atoms = 1,
    max_body_atoms = 7, 
    num_arrow_rules = 2,
    num_causes_rules = 4
    }   

-- I cannot get the following to find a solution
-- I am not sure why not
-- It should be a nicer way of finding a data-driven solution
-- (where the tune is encapsulated in the state_pos facts)
-- 
-- ... but currently it does not work ...
alternative_frame_music :: Frame
alternative_frame_music = Frame {
    types = [T "object", T "sensor", T "finger", T "grid", T "loudness", T "state"],
    type_hierarchy = [
        (T "object", [T "finger", T "sensor", T "grid"])
        ],
    objects = [
        (O "sensor_c", T "sensor"),
        (O "sensor_d", T "sensor"),
        (O "sensor_e", T "sensor"),
        (O "sensor_f", T "sensor"),
        (O "sensor_g", T "sensor"),
        (O "sensor_a", T "sensor"),
        (O "sensor_b", T "sensor"),
        (O "sensor_high_c", T "sensor"),
        (O "finger", T "finger"),
        (O "grid", T "grid"),
        (O "loudness_0", T "loudness"),
        (O "loudness_1", T "loudness"),
        (O "loudness_2", T "loudness"),
        (O "state_1", T "state"),
        (O "state_2", T "state"),
        (O "state_3", T "state"),
        (O "state_4", T "state"),
        (O "state_5", T "state"),
        (O "state_6", T "state"),
        (O "state_7", T "state"),
        (O "state_8", T "state")
        ],    
    exogeneous_objects = [],
    permanent_concepts = [
        (P "min_loudness", Given, [T "loudness"]),
        (P "max_loudness", Given, [T "loudness"]),
        (P "succ", Given, [T "loudness", T "loudness"]),
        (P "part_s", Given, [T "sensor", T "grid"]),
        (P "next_state", Given, [T "state", T "state"]),
        (P "state_pos", Constructed, [T "state", T "sensor"]),
        (P "state_p1", Constructed, [T "state"]),
        (P "state_p2", Constructed, [T "state"])
        ],
    fluid_concepts = [
        (C "loudness", [T "sensor", T "loudness"]), 
        (C "part_f", [T "finger", T "sensor"]),
        (C "current", [T "state"]), 
        (C "s1", [T "sensor"]),
        (C "s2", [T "sensor"]),
        (C "p1", [T "finger"]),
        (C "p2", [T "finger"]),
        (C "p3", [T "finger"]),
        (C "p4", [T "finger"])
        ],
    input_concepts = [C "loudness"],
    vars = [
        (V "f",       T "finger"),
        (V "l",       T "loudness"),
        (V "l2",      T "loudness"),
        (V "s",       T "sensor"),
        (V "s2",      T "sensor"),
        (V "x",       T "state")
        ],
    var_groups = [
        [V "l", V "s"],
        [V "f"],
        [V "f", V "s"],
        [V "f", V "s", V "s2"],
        [V "l", V "l2", V "s"],
        [V "f", V "x"],
        [V "f", V "s", V "s2", V "x"]
        ],
    aux_files = ["loudness.lp", "small_space.lp", "aux_1_1.lp", "state.lp"]
}

alternative_template_music :: Template
alternative_template_music = Template {
    dir = "music",
    frame = alternative_frame_music,
    min_body_atoms = 1,
    max_body_atoms = 4, 
    num_arrow_rules = 4,
    num_causes_rules = 8
    }    

-------------------------------------------------------------------------------
-- Rhythm templates
-------------------------------------------------------------------------------

frame_rhythm :: Frame
frame_rhythm = Frame {
    types = [T "object", T "sensor", T "grid", T "loudness", T "num"],
    type_hierarchy = [
        (T "object", [T "sensor", T "grid"])
        ],
    objects = [
        (O "sensor_bass_drum", T "sensor"),
        (O "sensor_snare_drum", T "sensor"),
        (O "sensor_hi_hat", T "sensor"),
        (O "grid", T "grid"),
        (O "loudness_0", T "loudness"),
        (O "loudness_1", T "loudness"),
        (O "loudness_2", T "loudness"),
        (O "loudness_3", T "loudness"),
        (O "num_0", T "num"),
        (O "num_1", T "num"),
        (O "num_2", T "num"),
        (O "num_3", T "num")
        ],  
    exogeneous_objects = [O "loudness_0", O "loudness_1", O "loudness_2", O "loudness_3", O "num_0", O "num_1", O "num_2", O "num_3"],
    permanent_concepts = [
        (P "min_loudness", Given, [T "loudness"]),
        (P "max_loudness", Given, [T "loudness"]),
        (P "succ", Given, [T "loudness", T "loudness"]),
        (P "part_s", Given, [T "sensor", T "grid"]),
        (P "is_bass_drum", Given, [T "sensor"]),        
        (P "is_snare_drum", Given, [T "sensor"]),        
        (P "is_hi_hat", Given, [T "sensor"]),
        (P "is_0", Given, [T "num"]),
        (P "is_1", Given, [T "num"]),
        (P "is_2", Given, [T "num"]),
        (P "is_3", Given, [T "num"]),
        (P "num_succ", Given, [T "num", T "num"])
        ],
    fluid_concepts = [
        (C "loudness", [T "sensor", T "loudness"]), 
        (C "q1", [T "sensor"]),
        (C "q2", [T "sensor"]),
        (C "q3", [T "sensor"]),
        (C "q4", [T "sensor"]),
        (C "val", [T "sensor", T "num"])
        ],
    input_concepts = [C "loudness"],
    vars = [
        (V "l",       T "loudness"),
        (V "l2",      T "loudness"),
        (V "n",       T "num"),
        (V "n2",      T "num"),
        (V "s",       T "sensor")
        ],
    var_groups = [
        [V "s"],
        [V "n", V "n2", V "s"],
        [V "l", V "l2", V "s"]
        ],
    aux_files = ["rhythm_loudness.lp", "rhythm_space.lp", "aux_1_1.lp", "rhythm_drums.lp", "rhythm_numbers.lp"]
}

template_rhythm :: Template
template_rhythm = Template {
    dir = "rhythm",
    frame = frame_rhythm,
    min_body_atoms = 1,
    max_body_atoms = 5, 
    num_arrow_rules = 4,
    num_causes_rules = 8
    }   

-------------------------------------------------------------------------------
-- SW templates
-------------------------------------------------------------------------------

frame_sw_simple :: Int -> Frame
frame_sw_simple n = Frame {
    types = [
        T "cell", 
        T "sensor", 
        T "grid", 
        T "letter",
        T "object"
        ],
    type_hierarchy = [
        (T "object", [T "cell", T "sensor", T "grid"])
        ],
    objects = [
        (O "sensor", T "sensor"),
        (O "grid", T "grid"),
        (O "letter_a", T "letter"),
        (O "letter_b", T "letter"),
        (O "letter_c", T "letter"),
        (O "letter_d", T "letter"),
        (O "letter_e", T "letter"),
        (O "letter_f", T "letter")
        ],
    exogeneous_objects = [O "letter_a", O "letter_b", O "letter_c", O "letter_d", O "letter_e", O "letter_f"],
    permanent_concepts = [
        (P "first_letter", Given, [T "letter"]),
        (P "succ", Given, [T "letter", T "letter"]),
        (P "less", Given, [T "letter", T "letter"]),
        (P "part_c", Constructed, [T "cell", T "grid"]),
        (P "r", Constructed, [T "cell", T "cell"]),
        (P "p1", Constructed, [T "cell"]),
        (P "p2", Constructed, [T "cell"])
        ],
    fluid_concepts = [
        (C "letter", [T "sensor", T "letter"]), 
        (C "part_s", [T "sensor", T "cell"]),
        (C "x", [T "sensor", T "letter"]), 
        (C "y", [T "cell", T "letter"])
        ],
    input_concepts = [C "letter"],
    vars = [
        (V "c", T "cell"),
        (V "c2", T "cell"),
        (V "l", T "letter"),
        (V "l2", T "letter"),
        (V "s", T "sensor")
        ],    
    var_groups = [
        [V "c", V "l", V "s"],
        [V "c", V "c2", V "s"],
        [V "l", V "l2", V "s"],
        [V "c", V "l", V "l2", V "s"]
        ],
    aux_files = ["letters.lp", "aux_1_1.lp"]
}

template_sw_simple :: Int -> Template
template_sw_simple n = Template {
    dir = "sw",
    frame = frame_sw_simple n,
    min_body_atoms = 1,
    max_body_atoms = 4, 
    num_arrow_rules = 1,
    num_causes_rules = 2
    }   

frame_sw_complex :: Int -> Frame
frame_sw_complex n = Frame {
    types = [
        T "cell", 
        T "sensor", 
        T "grid", 
        T "letter",
        T "object"
        ],
    type_hierarchy = [
        (T "object", [T "cell", T "sensor", T "grid"])
        ],
    objects = [
        (O "sensor", T "sensor"),
        (O "grid", T "grid"),
        (O "letter_a", T "letter"),
        (O "letter_b", T "letter"),
        (O "letter_c", T "letter"),
        (O "letter_d", T "letter"),
        (O "letter_e", T "letter"),
        (O "letter_f", T "letter")
        ],
    exogeneous_objects = [O "letter_a", O "letter_b", O "letter_c", O "letter_d", O "letter_e", O "letter_f"],
    permanent_concepts = [
        (P "first_letter", Given, [T "letter"]),
        (P "succ", Given, [T "letter", T "letter"]),
        (P "less", Given, [T "letter", T "letter"]),
        (P "part_c", Constructed, [T "cell", T "grid"]),
        (P "r", Constructed, [T "cell", T "cell"]),
        (P "p1", Constructed, [T "cell"]),
        (P "p2", Constructed, [T "cell"]),
        (P "p3", Constructed, [T "cell"]),
        (P "p4", Constructed, [T "cell"])
        ],
    fluid_concepts = [
        (C "letter", [T "sensor", T "letter"]), 
        (C "part_s", [T "sensor", T "cell"]),
        (C "c1", [T "sensor"]), 
        (C "c2", [T "sensor"]), 
        (C "x", [T "sensor", T "letter"]), 
        (C "y", [T "cell", T "letter"]), 
        (C "z", [T "cell", T "letter"])
        ],
    input_concepts = [C "letter"],
    vars = [
        (V "c", T "cell"),
        (V "c2", T "cell"),
        (V "g", T "grid"),
        (V "l", T "letter"),
        (V "l2", T "letter"),
        (V "s", T "sensor")
        ],    
    var_groups = [
        [V "c"],
        [V "s"],
        [V "c", V "s"],
        [V "c", V "g"],
        [V "l", V "s"],
        [V "c", V "l"],
        [V "c", V "c2"],
        [V "c", V "l", V "s"],
        [V "l", V "l2", V "s"],
        [V "c", V "l", V "l2", V "s"],
        [V "c", V "c2", V "s"],
        [V "c", V "c2", V "l", V "l2", V "s"]
        ],
    aux_files = ["letters.lp", "aux_1_1.lp"]
}

template_sw_complex :: Int -> Template
template_sw_complex n = Template {
    dir = "sw",
    frame = frame_sw_complex n,
    min_body_atoms = 3,
    max_body_atoms = 6, 
    num_arrow_rules = 3,
    num_causes_rules = 5
    }   

-------------------------------------------------------------------------------
-- Occlusion template
-------------------------------------------------------------------------------

frame_occlusion :: Int -> Int -> Frame
frame_occlusion num_cells num_movers = Frame {
    types = [T "cell", T "mover"],
    type_hierarchy = [],
    objects = [(O ("c" ++ show i), T "cell") | i <- [1..num_cells]] ++ 
                [(O ("m" ++ show i), T "mover") | i <- [1..num_movers]],
    exogeneous_objects = [],
    permanent_concepts = [
        (P "right", Given, [T "cell", T "cell"]),
        (P "p1", Constructed, [T "mover"]),
        (P "p2", Constructed, [T "mover"]),
        (P "p3", Constructed, [T "mover"])
    ],
    fluid_concepts = [
        (C "in", [T "mover", T "cell"]), 
        (C "s1", [T "mover"]),
        (C "s2", [T "mover"]),
        (C "s3", [T "mover"])
        ],
    input_concepts = [C "in"],
    vars = [
        (V "c1", T "cell"),
        (V "c2", T "cell"),
        (V "x", T "mover")
        ],
    var_groups = [
        [V "c1", V "c2", V "x"]
        ],
    aux_files = []
}    

template_occlusion :: String -> Int -> Int -> Template
template_occlusion f num_cells num_movers = Template {
    dir = "misc",
    frame = frame_occlusion num_cells num_movers,
    min_body_atoms = 1,
    max_body_atoms = 4, 
    num_arrow_rules = 0,
    num_causes_rules = 8
    }    

-------------------------------------------------------------------------------
-- Binding template
-- 
-- This is the frame for the binding examples.
-- 
-- The frame contains cells, light sensors, and touch sensors.
-------------------------------------------------------------------------------

frame_binding :: Frame
frame_binding = Frame {
    types = [T "cell", T "light_sensor", T "touch_sensor", T "touch"],
    type_hierarchy = [],
    objects = [(O ("cell_" ++ show i), T "cell") | i <- [1..11]] ++ 
                [(O ("light_sensor_" ++ show i), T "light_sensor") | i <- [1..11]] ++ 
                [(O ("touch_" ++ show i), T "touch") | i <- [0..3]] ++ 
                [(O ("touch_sensor_" ++ show i), T "touch_sensor") | i <- [1..2]],
    exogeneous_objects = [O ("touch_" ++ show i) | i <- [0..3]],
    permanent_concepts = [
        (P "r", Given, [T "cell", T "cell"]),
        (P "min_touch", Given, [T "touch"]),
        (P "max_touch", Given, [T "touch"]),
        (P "succ", Given, [T "touch", T "touch"]),
        (P "in_light", Constructed, [T "light_sensor", T "cell"]),
        (P "in_touch", Constructed, [T "touch_sensor", T "cell"])
    ],
    fluid_concepts = [
        (C "on", [T "cell"]), 
        (C "off", [T "cell"]),
        (C "black", [T "light_sensor"]), 
        (C "white", [T "light_sensor"]),
        (C "touch", [T "touch_sensor", T "touch"]),
        (C "decay", [T "touch_sensor", T "touch"])
        ],
    input_concepts = [C "black", C "white", C "touch"],
    vars = [
        (V "c", T "cell"),
        (V "c2", T "cell"),
        (V "c3", T "cell"),
        (V "x", T "touch_sensor"),
        (V "y", T "light_sensor"),
        (V "l", T "touch"),
        (V "l2", T "touch")
        ],
    var_groups = [
        [V "c", V "c2", V "c3"],
        [V "l", V "x"],
        [V "l", V "l2", V "x"],
        [V "c", V "l", V "x"],
        [V "c", V "y"]
        ],
    aux_files = ["aux_eca_space11.lp", "aux_eca_touch.lp", "aux_eca_binding.lp"]
}

template_binding :: Template
template_binding = Template {
    dir = "misc",
    frame = frame_binding,
    min_body_atoms = 1,
    max_body_atoms = 5, 
    num_arrow_rules = 4,
    num_causes_rules = 4
    }    
