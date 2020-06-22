module ExampleTemplates where

import Interpretation
import SolveTemplates
    
-------------------------------------- Templates ------------------------------

misc_templates :: [(String, (String, Template, String))]
misc_templates = [
    ("predict_1.lp", ("data/misc", template_misc_1_1, "predict_1.lp")),
    ("predict_2.lp", ("data/misc", template_misc_2_1, "predict_2.lp")),
    ("predict_3.lp", ("data/misc", template_misc_3_1, "predict_3.lp")),
    ("predict_4.lp", ("data/misc", template_misc_4_1, "predict_4.lp")),
    ("predict_example.lp", ("data/misc", template_misc_4_mislabel, "predict_example.lp")),
    ("predict_mislabel.lp", ("data/misc", template_misc_4_mislabel, "predict_mislabel.lp")),
    ("exog_1.lp", ("data/misc", template_exog_1, "exog_1.lp")),
    ("first_last_1.lp", ("data/misc", template_first_last_1, "first_last_1.lp")),
    ("first_last_2.lp", ("data/misc", template_first_last_1, "first_last_2.lp")),
    ("first_last_3.lp", ("data/misc", template_first_last_3, "first_last_3.lp")),
    ("first_last_4.lp", ("data/misc", template_first_last_3, "first_last_4.lp")),
    ("first_last_5.lp", ("data/misc", template_first_last_5, "first_last_5.lp")),
    ("first_last_6.lp", ("data/misc", template_first_last_6, "first_last_6.lp")),
    ("second_last_1.lp", ("data/misc", template_second_last_1, "second_last_1.lp")),
    ("second_last_2.lp", ("data/misc", template_second_last_2, "second_last_2.lp"))
    ]

frame_misc_1_1 :: Frame    
frame_misc_1_1 = Frame {
    types = [T "object"],
    type_hierarchy = [],
    objects = [
        (O "sensor_a", T "object")
        ],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [
        (C "on", [T "object"]), 
        (C "off", [T "object"])
        ],
    input_concepts = [C "on", C "off"],
    static_concepts = [],
    vars = [
        (V "x", T "object")
        ],
    var_groups = [
        [V "x"]
        ],
    aux_files = []
}

template_misc_1_1 :: Template
template_misc_1_1 = Template {
    dir = "misc",
    frame = frame_misc_1_1,
    min_body_atoms = 1,
    max_body_atoms = 1, 
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

frame_misc_2_1 :: Frame
frame_misc_2_1 = Frame {
    types = [T "sensor_1", T "sensor_2", T "cell", T "grid", T "object"],
    type_hierarchy = [
        (T "object", [T "cell", T "grid"]),
        (T "cell", [T "sensor_1", T "sensor_2"])
        ],
    objects = [
        (O "sensor_a", T "sensor_1"),
        (O "sensor_b", T "sensor_2"),
        (O "grid", T "grid")
        ],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [
        (C "on", [T "sensor_1"]), 
        (C "off", [T "sensor_1"]),
        (C "a", [T "sensor_2"]),
        (C "b", [T "sensor_2"]),
        (C "c", [T "sensor_2"]),
        (C "part", [T "cell", T "grid"])
        ],
    input_concepts = [C "on", C "off", C "a", C "b", C "c"],
    static_concepts = [],
    vars = [
        (V "s1", T "sensor_1"),
        (V "s2", T "sensor_2"),
        (V "c", T "cell"),
        (V "g", T "grid"),
        (V "x", T "object")
        ],
    var_groups = [
        [V "x"],
        [V "s1"],
        [V "s2"],
        [V "c"],
        [V "c", V "g"]
        ],
    aux_files = []
}

template_misc_2_1 :: Template
template_misc_2_1 = Template {
    dir = "misc",
    frame = frame_misc_2_1,
    min_body_atoms = 1,
    max_body_atoms = 1, 
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

frame_misc_3_1 :: Frame
frame_misc_3_1 = Frame {
    types = [T "object"],
    type_hierarchy = [],
    objects = [
        (O "sensor_a", T "object")
        ],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [
        (C "a", [T "object"]), 
        (C "b", [T "object"]),
        (C "c", [T "object"]),
        (C "d", [T "object"])
        ],
    input_concepts = [C "a", C "b", C "c", C "d"],
    static_concepts = [],
    vars = [
        (V "x", T "object")
        ],
    var_groups = [
        [V "x"]
        ],
    aux_files = []
}

template_misc_3_1 :: Template
template_misc_3_1 = Template {
    dir = "misc",
    frame = frame_misc_3_1,
    min_body_atoms = 1,
    max_body_atoms = 1, 
    num_arrow_rules = 0,
    num_causes_rules = 4,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

frame_misc_4_1 :: Frame
frame_misc_4_1 = Frame {
    types = [
        T "sensor", 
        T "grid", 
        T "letter",
        T "object"
        ],
    type_hierarchy = [
        (T "object", [T "sensor", T "grid"])
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
    exogeneous_objects = [],
    permanent_concepts = [
        (P "succ", Constructed, [T "letter", T "letter"]),
        (P "part", Constructed, [T "sensor", T "grid"])
        ],
    fluid_concepts = [
        (C "letter", [T "sensor", T "letter"])
        ],
    input_concepts = [C "letter"],
    static_concepts = [],
    vars = [
        (V "l", T "letter"),
        (V "l2", T "letter"),
        (V "s", T "sensor")
        ],    
    var_groups = [
        [V "l", V "l2", V "s"]
        ],
    aux_files = []
}

template_misc_4_1 :: Template
template_misc_4_1 = Template {
    dir = "misc",
    frame = frame_misc_4_1,
    min_body_atoms = 2,
    max_body_atoms = 2, 
    num_arrow_rules = 0,
    num_causes_rules = 1,
    num_visual_predicates = Nothing,
    use_noise = False
    }   

frame_misc_5_1 :: Frame    
frame_misc_5_1 = Frame {
    types = [T "object"],
    type_hierarchy = [],
    objects = [
        (O "sensor_a", T "object")
        ],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [
        (C "on", [T "object"]), 
        (C "off", [T "object"]),
        (C "a", [T "object"]),
        (C "b", [T "object"]),
        (C "c", [T "object"]),
        (C "d", [T "object"])
        ],
    input_concepts = [C "on", C "off"],
    static_concepts = [],
    vars = [
        (V "x", T "object")
        ],
    var_groups = [
        [V "x"]
        ],
    aux_files = []
}

template_misc_5_1 :: Template
template_misc_5_1 = Template {
    dir = "misc",
    frame = frame_misc_5_1,
    min_body_atoms = 1,
    max_body_atoms = 1, 
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

frame_exog_1 :: Frame    
frame_exog_1 = Frame {
    types = [T "object"],
    type_hierarchy = [],
    objects = [
        (O "a", T "object")
        ],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [
        (C "on", [T "object"]), 
        (C "off", [T "object"]),
        (C "bong", [T "object"]),
        (C "noop", [T "object"])
        ],
    input_concepts = [C "on", C "off", C "bong", C "noop"],
    static_concepts = [],
    vars = [
        (V "x", T "object")
        ],
    var_groups = [
        [V "x"]
        ],
    aux_files = []
}

template_exog_1 :: Template
template_exog_1 = Template {
    dir = "misc",
    frame = frame_exog_1,
    min_body_atoms = 1,
    max_body_atoms = 1, 
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

frame_misc_4_mislabel :: Frame    
frame_misc_4_mislabel = Frame {
    types = [T "object"],
    type_hierarchy = [],
    objects = [
        (O "sensor_a", T "object"),
        (O "sensor_b", T "object")
        ],
    exogeneous_objects = [],
    permanent_concepts = [(P "r", Constructed, [T "object", T "object"])],
    fluid_concepts = [
        (C "on", [T "object"]), 
        (C "off", [T "object"]),
        (C "p1", [T "object"]),
        (C "p2", [T "object"]),
        (C "p3", [T "object"])
        ],
    input_concepts = [C "on", C "off"],
    static_concepts = [],
    vars = [
        (V "x", T "object")
        ],
    var_groups = [
        [V "x"]
        ],
    aux_files = []
}

template_misc_4_mislabel :: Template
template_misc_4_mislabel = Template {
    dir = "misc",
    frame = frame_misc_4_mislabel,
    min_body_atoms = 1,
    max_body_atoms = 1, 
    num_arrow_rules = 3,
    num_causes_rules = 3,
    num_visual_predicates = Nothing,
    use_noise = True
    }   

frame_first_last_1 :: Frame    
frame_first_last_1 = Frame {
    types = [T "sensor"],
    type_hierarchy = [],
    objects = [
        (O "cell_1", T "sensor"),
        (O "cell_2", T "sensor"),
        (O "cell_3", T "sensor")
        ],
    exogeneous_objects = [],
    permanent_concepts = [(P "right", Given, [T "sensor", T "sensor"])],
    fluid_concepts = [
        (C "on", [T "sensor"]), 
        (C "off", [T "sensor"])
        ],
    input_concepts = [C "on", C "off"],
    static_concepts = [],
    vars = [
        (V "x", T "sensor"),
        (V "y", T "sensor")
        ],
    var_groups = [
        [V "x", V "y"]
        ],
    aux_files = ["misc_first_last.lp"]
}

template_first_last_1 :: Template
template_first_last_1 = Template {
    dir = "misc",
    frame = frame_first_last_1,
    min_body_atoms = 1,
    max_body_atoms = 2, 
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

frame_first_last_3 :: Frame    
frame_first_last_3 = Frame {
    types = [T "sensor"],
    type_hierarchy = [],
    objects = [
        (O "cell_1", T "sensor"),
        (O "cell_2", T "sensor"),
        (O "cell_3", T "sensor")
        ],
    exogeneous_objects = [],
    permanent_concepts = [(P "right", Given, [T "sensor", T "sensor"])],
    fluid_concepts = [
        (C "on", [T "sensor"]), 
        (C "off", [T "sensor"])
        ],
    input_concepts = [C "on", C "off"],
    static_concepts = [],
    vars = [
        (V "x", T "sensor"),
        (V "y", T "sensor")
        ],
    var_groups = [
        [V "x", V "y"]
        ],
    aux_files = ["misc_first_last.lp", "bnn.lp"]
}

template_first_last_3 :: Template
template_first_last_3 = Template {
    dir = "misc",
    frame = frame_first_last_3,
    min_body_atoms = 1,
    max_body_atoms = 2, 
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }    

frame_first_last_5 :: Frame    
frame_first_last_5 = Frame {
    types = [T "sensor"],
    type_hierarchy = [],
    objects = [
        (O "cell_1", T "sensor"),
        (O "cell_2", T "sensor"),
        (O "cell_3", T "sensor")
        ],
    exogeneous_objects = [],
    permanent_concepts = [(P "right", Given, [T "sensor", T "sensor"])],
    fluid_concepts = [
        (C "p", [T "sensor"]), 
        (C "q", [T "sensor"])
        ],
    input_concepts = [C "p", C "q"],
    static_concepts = [],
    vars = [
        (V "x", T "sensor"),
        (V "y", T "sensor")
        ],
    var_groups = [
        [V "x", V "y"]
        ],
    aux_files = ["misc_first_last_5.lp", "bnn.lp"]
}

template_first_last_5 :: Template
template_first_last_5 = Template {
    dir = "misc",
    frame = frame_first_last_5,
    min_body_atoms = 1,
    max_body_atoms = 2, 
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }        

frame_first_last_6 :: Frame    
frame_first_last_6 = Frame {
    types = [T "sensor", T "space"],
    type_hierarchy = [],
    objects = [
        (O "cell_1", T "sensor"),
        (O "cell_2", T "sensor"),
        (O "cell_3", T "sensor"),
        (O "space_1", T "space"),
        (O "space_2", T "space"),
        (O "space_3", T "space"),
        (O "space_whole", T "space")
        ],
    exogeneous_objects = [],
    permanent_concepts = [
        (P "right", Given, [T "space", T "space"]),
        (P "in", Given, [T "sensor", T "space"]),
        (P "in2", Given, [T "space", T "space"])
        ],
    fluid_concepts = [
        (C "p", [T "sensor"]), 
        (C "q", [T "sensor"])
        ],
    input_concepts = [C "p", C "q"],
    static_concepts = [],
    vars = [
        (V "x", T "sensor"),
        (V "y", T "sensor"),
        (V "s1", T "space"),
        (V "s2", T "space")
        ],
    var_groups = [
        [V "x", V "y"],
        [V "x", V "y", V "s1", V "s2"]
        ],
    aux_files = ["misc_first_last_5.lp", "bnn.lp"]
}

template_first_last_6 :: Template
template_first_last_6 = Template {
    dir = "misc",
    frame = frame_first_last_6,
    min_body_atoms = 1,
    max_body_atoms = 4, 
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False
    }      

frame_second_last_1 :: [String] -> Frame    
frame_second_last_1 auxes = Frame {
    types = [T "object", T "cell", T "sensor", T "value"],
    type_hierarchy = [
        (T "object", [T "cell", T "sensor", T "value"])
        ],
    objects = [
        (O "s", T "sensor"),
        (O "a", T "cell"),
        (O "b", T "cell"),
        (O "value_1", T "value"),
        (O "value_2", T "value"),
        (O "value_3", T "value"),
        (O "value_4", T "value"),
        (O "value_5", T "value"),
        (O "value_6", T "value"),
        (O "value_7", T "value"),
        (O "value_8", T "value"),
        (O "value_9", T "value")
        ],
    exogeneous_objects = [],
    permanent_concepts = [
        (P "greater", Given, [T "value", T "value"]),
        (P "left", Given, [T "sensor", T "cell"]),
        (P "right", Given, [T "sensor", T "cell"])
        ],
    fluid_concepts = [
        (C "p", [T "sensor"]), 
        (C "q", [T "sensor"]),
        (C "value", [T "cell", T "value"])
        ],
    input_concepts = [C "value"],
    static_concepts = [],
    vars = [
        (V "s", T "sensor"),
        (V "c1", T "cell"),
        (V "c2", T "cell"),
        (V "v1", T "value"),
        (V "v2", T "value")
        ],
    var_groups = [
        [V "s", V "c1", V "c2", V "v1", V "v2"]
        ],
    aux_files = auxes
}

template_second_last_1 :: Template
template_second_last_1 = Template {
    dir = "misc",
    frame = frame_second_last_1 [],
    min_body_atoms = 1,
    max_body_atoms = 5, 
    num_arrow_rules = 2,
    num_causes_rules = 0,
    num_visual_predicates = Nothing,
    use_noise = False
    }        

template_second_last_2 :: Template
template_second_last_2 = Template {
    dir = "misc",
    frame = frame_second_last_1 ["bnn.lp"],
    min_body_atoms = 1,
    max_body_atoms = 5, 
    num_arrow_rules = 2,
    num_causes_rules = 0,
    num_visual_predicates = Nothing,
    use_noise = False
    }            