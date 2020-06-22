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
    static_concepts = [],
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
    num_causes_rules = 4,
    num_visual_predicates = Nothing,
    use_noise = False
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
    static_concepts = [],
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
    num_causes_rules = 3,
    num_visual_predicates = Nothing,
    use_noise = False
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
    static_concepts = [],
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
    num_causes_rules = 4,
    num_visual_predicates = Nothing,
    use_noise = False
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
    static_concepts = [],
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
    num_causes_rules = 8,
    num_visual_predicates = Nothing,
    use_noise = False
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
    static_concepts = [],
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
    num_causes_rules = 8,
    num_visual_predicates = Nothing,
    use_noise = False
    }   

-------------------------------------------------------------------------------
-- SW templates
-------------------------------------------------------------------------------

frame_book :: Int -> Frame
frame_book p = Frame {
    types = [T "sensor"],
    type_hierarchy = [],
    objects = [(O "sensor", T "sensor")],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [(C "0", [T "sensor"]), (C "1", [T "sensor"])] ++ 
        map (\i -> (C ("p_" ++ show i), [T "sensor"])) [1 .. p],
    input_concepts = [],
    static_concepts = [],
    vars = [
        (V "x", T "sensor")
        ],    
    var_groups = [[V "x"]],
    aux_files = []
}

book_template :: Int -> Int -> Template
book_template p r = Template {
    dir = "teaching-size",
    frame = frame_book p,
    min_body_atoms = 1,
    max_body_atoms = 3, 
    num_arrow_rules = r,
    num_causes_rules = r,
    num_visual_predicates = Nothing,
    use_noise = False
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
    static_concepts = [],
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
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
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
    static_concepts = [],
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
    num_causes_rules = 5,
    num_visual_predicates = Nothing,
    use_noise = False
    }   

-------------------------------------------------------------------------------
-- seq-mnist templates
-------------------------------------------------------------------------------

frame_seq_mnist_simple :: Int -> Frame
frame_seq_mnist_simple n = Frame {
    types = [
        T "cell", 
        T "sensor", 
        T "grid", 
        T "number",
        T "object"
        ],
    type_hierarchy = [
        (T "object", [T "cell", T "sensor", T "grid"])
        ],
    objects = [
        (O "sensor", T "sensor"),
        (O "grid", T "grid"),
        (O "number_0", T "number"),
        (O "number_1", T "number"),
        (O "number_2", T "number"),
        (O "number_3", T "number"),
        (O "number_4", T "number"),
        (O "number_5", T "number"),
        (O "number_6", T "number"),
        (O "number_7", T "number"),
        (O "number_8", T "number"),
        (O "number_9", T "number")
        ],
    exogeneous_objects = [O "number_0", O "number_1", O "number_2", O "number_3", O "number_4", O "number_5", O "number_6", O "number_7", O "number_8", O "number_9"],
    permanent_concepts = [
        (P "first_number", Given, [T "number"]),
        (P "succ", Given, [T "number", T "number"]),
        (P "less", Given, [T "number", T "number"]),
        (P "part_c", Constructed, [T "cell", T "grid"]),
        (P "r", Constructed, [T "cell", T "cell"]),
        (P "p1", Constructed, [T "cell"]),
        (P "p2", Constructed, [T "cell"])
        ],
    fluid_concepts = [
        (C "value", [T "sensor", T "number"]), 
        (C "part_s", [T "sensor", T "cell"]),
        (C "x", [T "sensor", T "number"]), 
        (C "y", [T "cell", T "number"])
        ],
    input_concepts = [C "value"],
    static_concepts = [],
    vars = [
        (V "c", T "cell"),
        (V "c2", T "cell"),
        (V "s", T "sensor"),
        (V "x", T "number"),
        (V "y", T "number")
        ],    
    var_groups = [
        [V "c", V "s", V "x"],
        [V "c", V "c2", V "s"],
        [V "s", V "x", V "y"],
        [V "c", V "s", V "x", V "y"]
        ],
    aux_files = ["numbers.lp", "seq_mnist.lp", "aux_1_1.lp"]
}

template_seq_mnist_simple :: Int -> Template
template_seq_mnist_simple n = Template {
    dir = "seq-mnist",
    frame = frame_seq_mnist_simple n,
    min_body_atoms = 1,
    max_body_atoms = 4, 
    num_arrow_rules = 1,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }   

frame_seq_mnist_complex :: Int -> Frame
frame_seq_mnist_complex n = Frame {
    types = [
        T "cell", 
        T "sensor", 
        T "grid", 
        T "number",
        T "object"
        ],
    type_hierarchy = [
        (T "object", [T "cell", T "sensor", T "grid"])
        ],
    objects = [
        (O "sensor", T "sensor"),
        (O "grid", T "grid"),
        (O "number_0", T "number"),
        (O "number_1", T "number"),
        (O "number_2", T "number"),
        (O "number_3", T "number"),
        (O "number_4", T "number"),
        (O "number_5", T "number"),
        (O "number_6", T "number"),
        (O "number_7", T "number"),
        (O "number_8", T "number"),
        (O "number_9", T "number")
        ],
    exogeneous_objects = [O "number_0", O "number_1", O "number_2", O "number_3", O "number_4", O "number_5", O "number_6", O "number_7", O "number_8", O "number_9"],
    permanent_concepts = [
        (P "first_number", Given, [T "number"]),
        (P "succ", Given, [T "number", T "number"]),
        (P "less", Given, [T "number", T "number"]),
        (P "part_c", Constructed, [T "cell", T "grid"]),
        (P "r", Constructed, [T "cell", T "cell"]),
        (P "p1", Constructed, [T "cell"]),
        (P "p2", Constructed, [T "cell"]),
        (P "p3", Constructed, [T "cell"]),
        (P "p4", Constructed, [T "cell"])
        ],
    fluid_concepts = [
        (C "value", [T "sensor", T "number"]), 
        (C "part_s", [T "sensor", T "cell"]),
        (C "c1", [T "sensor"]), 
        (C "c2", [T "sensor"]), 
        (C "x", [T "sensor", T "number"]), 
        (C "y", [T "cell", T "number"]), 
        (C "z", [T "cell", T "number"])
        ],
    input_concepts = [C "value"],
    static_concepts = [],
    vars = [
        (V "c", T "cell"),
        (V "c2", T "cell"),
        (V "s", T "sensor"),
        (V "x", T "number"),
        (V "y", T "number")
        ],    
    var_groups = [
        [V "c", V "s", V "x"],
        [V "c", V "c2", V "s"],
        [V "s", V "x", V "y"],
        [V "c", V "s", V "x", V "y"]
        ],
    aux_files = ["numbers.lp", "seq_mnist.lp", "aux_1_1.lp"]
}

template_seq_mnist_complex :: Int -> Template
template_seq_mnist_complex n = Template {
    dir = "seq-mnist",
    frame = frame_seq_mnist_complex n,
    min_body_atoms = 3,
    max_body_atoms = 6, 
    num_arrow_rules = 3,
    num_causes_rules = 5,
    num_visual_predicates = Nothing,
    use_noise = False
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
    static_concepts = [],
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
    num_causes_rules = 8,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

-------------------------------------------------------------------------------
-- Walker template
-------------------------------------------------------------------------------

frame_walker :: Int -> Int -> Int -> Frame
frame_walker max_x max_y num_movers = Frame {
    types = [T "cell", T "mover"],
    type_hierarchy = [],
    objects = [(O ("c_" ++ show i ++ "_" ++ show j), T "cell") | i <- [0..max_x-1], j <- [0..max_y-1]] ++ 
                [(O ("w" ++ show i), T "mover") | i <- [1..num_movers]],
    exogeneous_objects = [],
    permanent_concepts = [
        (P "right", Given, [T "cell", T "cell"]),
        (P "below", Given, [T "cell", T "cell"])
    ],
    fluid_concepts = [
        (C "in", [T "mover", T "cell"]), 
        (C "s1", [T "mover"]),
        (C "s2", [T "mover"]),
        (C "s3", [T "mover"]),
        (C "s4", [T "mover"]),
        (C "s5", [T "mover"]),
        (C "s6", [T "mover"]),
        (C "s7", [T "mover"]),
        (C "s8", [T "mover"]),
        (C "s9", [T "mover"]),
        (C "s10", [T "mover"])
        ],
    input_concepts = [C "in"],
    static_concepts = [],
    vars = [
        (V "c1", T "cell"),
        (V "c2", T "cell"),
        (V "x", T "mover")
        ],
    var_groups = [
        [V "x"],
        [V "c1", V "c2", V "x"]
        ],
    aux_files = ["bnn.lp", "visual_walker.lp"]
}    

template_walker :: Int -> Int -> Int -> Template
template_walker max_x max_y num_movers = Template {
    dir = "misc",
    frame = frame_walker max_x max_y  num_movers,
    min_body_atoms = 1,
    max_body_atoms = 3, 
    num_arrow_rules = 0,
    num_causes_rules = 18,
    num_visual_predicates = Just num_movers,
    use_noise = False
    }    

-------------------------------------------------------------------------------
-- Noisy template
-- 
-- This is the frame for the noisy sequence examples.
-------------------------------------------------------------------------------

frame_noisy :: Int -> Int -> Frame
frame_noisy num_ps extra_ps = Frame {
    types = [T "sensor"],
    type_hierarchy = [],
    objects = [(O ("sensor"), T "sensor")],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [(C (show i), [T "sensor"]) | i <- [1 .. num_ps]] ++ 
        map (\i -> (C ("p_" ++ show i), [T "sensor"])) [1 .. extra_ps],
    input_concepts = [C (show i) | i <- [1 .. num_ps]],
    static_concepts = [],
    vars = [
        (V "x", T "sensor")
        ],    
    var_groups = [[V "x"]],
    aux_files = ["bnn.lp"]
}

template_noisy :: Int -> Int -> Int -> Template
template_noisy num_ps p r = Template {
    dir = "noisy",
    frame = frame_noisy num_ps p,
    min_body_atoms = 1,
    max_body_atoms = 1, 
    num_arrow_rules = r,
    num_causes_rules = num_ps + r,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

-------------------------------------------------------------------------------
-- Mislabel template
-- 
-- This is the frame for the mislabelled sequence examples.
-------------------------------------------------------------------------------

frame_mislabel :: Int -> Int -> Frame
frame_mislabel num_ps extra_ps = Frame {
    types = [T "sensor"],
    type_hierarchy = [],
    objects = [(O ("sensor"), T "sensor")],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [(C (show i), [T "sensor"]) | i <- [1 .. num_ps]] ++ 
        map (\i -> (C ("p_" ++ show i), [T "sensor"])) [1 .. extra_ps],
    input_concepts = [C (show i) | i <- [1 .. num_ps]],
    static_concepts = [],
    vars = [
        (V "x", T "sensor")
        ],    
    var_groups = [[V "x"]],
    aux_files = ["mislabel.lp"]
}

template_mislabel :: Int -> Int -> Int -> Template
template_mislabel num_ps p r = Template {
    dir = "mislabel",
    frame = frame_mislabel num_ps p,
    min_body_atoms = 1,
    max_body_atoms = 2, 
    num_arrow_rules = r,
    num_causes_rules = num_ps + r,
    num_visual_predicates = Nothing,
    use_noise = True
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
    static_concepts = [],
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
    num_causes_rules = 4,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

-------------------------------------------------------------------------------
-- Sokoban template
-------------------------------------------------------------------------------

frame_sokoban :: Int -> Int -> Int -> Frame
frame_sokoban max_x max_y num_blocks = Frame {
    types = [T "cell", T "1", T "2"],
    type_hierarchy = [],
    objects = (O "x1", T "1") : [(O ("cell_" ++ show i ++ "_" ++ show j), T "cell") | i <- [1..max_x], j <- [1..max_y]] ++ [(O ("x" ++ show (i+1)), T "2") | i <- [1..num_blocks]],
    exogeneous_objects = [],
    permanent_concepts = [
        (P "right", Given, [T "cell", T "cell"]),
        (P "below", Given, [T "cell", T "cell"]),
        (P "is_not_wall", Given, [T "cell"]),
        (P "is_wall", Given, [T "cell"])
    ],
    fluid_concepts = [
        (C "in_1", [T "1", T "cell"]), 
        (C "in_2", [T "2", T "cell"]), 
        (C "noop", [T "1"]), 
        (C "north", [T "1"]), 
        (C "south", [T "1"]),
        (C "east", [T "1"]), 
        (C "west", [T "1"]), 
        (C "p1", [T "2"]),
        (C "p2", [T "2"]),
        (C "p3", [T "2"]),
        (C "p4", [T "2"])
        ],
    input_concepts = [C "in_1", C "in_2", C "noop", C "north", C "south", C "east", C "west", C "p1", C "p2", C "p3", C "p4"],
    static_concepts = [C "p1", C "p2", C "p3", C "p4"],
    vars = [
        (V "c1", T "cell"),
        (V "c2", T "cell"),
        (V "x", T "1"),
        (V "y", T "2")
        ],
    var_groups = [
        [V "c1", V "c2", V "x"],
        [V "c1", V "c2", V "x", V "y"]
        ],
    aux_files = []
}    

template_sokoban :: Int -> Int -> Int -> Template
template_sokoban max_x max_y num_blocks = Template {
    dir = "sokoban",
    frame = frame_sokoban max_x max_y num_blocks,
    min_body_atoms = 1,
    max_body_atoms = 4, 
    num_arrow_rules = 4,
    num_causes_rules = 8,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

-------------------------------------------------------------------------------
-- Sokoban from pixels template
-------------------------------------------------------------------------------

frame_sok_pixels :: Int -> Int -> Int -> Int -> Frame
frame_sok_pixels max_x max_y num_t1s num_t2s = Frame {
    types = [T "cell", T "1", T "2"],
    type_hierarchy = [],
    objects = [(O ("cell_" ++ show i ++ "_" ++ show j), T "cell") | i <- [1..max_x], j <- [1..max_y]] ++ t1s ++ t2s,
    exogeneous_objects = [],
    permanent_concepts = [
        (P "right", Given, [T "cell", T "cell"]),
        (P "below", Given, [T "cell", T "cell"]),
        (P "is_not_wall", Given, [T "cell"]),
        (P "is_wall", Given, [T "cell"])
    ],
    fluid_concepts = [
        (C "in_1", [T "1", T "cell"]), 
        (C "in_2", [T "2", T "cell"]), 
        (C "noop", [T "1"]), 
        (C "north", [T "1"]), 
        (C "south", [T "1"]),
        (C "east", [T "1"]), 
        (C "west", [T "1"]), 
        (C "p1", [T "2"]),
        (C "p2", [T "2"]),
        (C "p3", [T "2"]),
        (C "p4", [T "2"])
        ],
    input_concepts = [C "in_1", C "in_2", C "noop", C "north", C "south", C "east", C "west", C "p1", C "p2", C "p3", C "p4"],
    static_concepts = [C "p1", C "p2", C "p3", C "p4"],
    vars = [
        (V "c1", T "cell"),
        (V "c2", T "cell"),
        (V "x", T "1"),
        (V "y", T "2")
        ],
    var_groups = [
        [V "c1", V "c2", V "x"],
        [V "c1", V "c2", V "x", V "y"]
        ],
    aux_files = ["bnn.lp", "visual_sokoban.lp"]
} where
    t1s = map f [1 .. num_t1s]
    t2s = map g [1 .. num_t2s]
    f i = (O ("x" ++ show i), T "1")
    g i = (O ("x" ++ show (i + num_t1s)), T "2")
    
template_sok_pixels :: Int -> Int -> Int -> Int -> Template
template_sok_pixels max_x max_y num_t1s num_t2s = Template {
    dir = "sok-pixels",
    frame = frame_sok_pixels max_x max_y num_t1s num_t2s,
    min_body_atoms = 1,
    max_body_atoms = 4, 
    num_arrow_rules = 4,
    num_causes_rules = 8,
    num_visual_predicates = Just 2,
    use_noise = False
    }    

-------------------------------------------------------------------------------
-- House template
-------------------------------------------------------------------------------

frame_house :: Int -> Int -> Int -> Int -> Frame
frame_house v_x v_y w_x w_y = Frame {
    types = [T "v_cell", T "w_cell", T "man", T "nat"],
    type_hierarchy = [],
    objects = (O "man", T "man") : [(O "number_0", T "nat"), (O "number_1", T "nat")] ++ [(O ("w_c_" ++ show i ++ "_" ++ show j), T "w_cell") | i <- [1..w_x], j <- [1..w_y]] ++ [(O ("v_c_" ++ show i ++ "_" ++ show j), T "v_cell") | i <- [1..v_x], j <- [1..v_y]],
    exogeneous_objects = [O "number_0", O "number_1"],
    permanent_concepts = [
        (P "right", Given, [T "w_cell", T "w_cell"]),
        (P "below", Given, [T "w_cell", T "w_cell"]),
        (P "is_zero", Given, [T "nat"]),
        (P "is_one", Given, [T "nat"]),
        (P "on", Constructed, [T "w_cell"]),
        (P "off", Constructed, [T "w_cell"])
    ],
    fluid_concepts = [
        (C "in", [T "v_cell", T "w_cell"]),
        (C "intensity", [T "v_cell", T "nat"]),
        (C "noop", [T "man"]), 
        (C "north", [T "man"]), 
        (C "south", [T "man"]),
        (C "east", [T "man"]), 
        (C "west", [T "man"])
        ],
    input_concepts = [C "noop", C "north", C "south", C "east", C "west"],
    static_concepts = [],
    vars = [
        (V "m", T "man"),
        (V "v", T "v_cell"),
        (V "w", T "w_cell"),
        (V "w2", T "w_cell"),
        (V "n", T "nat")
        ],
    var_groups = [
        [V "n", V "v", V "w"],
        [V "m", V "v", V "w", V "w2"]
        ],
    aux_files = ["nat.lp"]
}    

template_house :: Int -> Int -> Int -> Int -> Template
template_house vx vy wx wy = Template {
    dir = "house",
    frame = frame_house vx vy wx wy,
    min_body_atoms = 1,
    max_body_atoms = 3, 
    num_arrow_rules = 2,
    num_causes_rules = 4,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

