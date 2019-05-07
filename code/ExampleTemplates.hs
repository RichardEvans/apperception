module ExampleTemplates where

import Interpretation
import SolveTemplates
    
-------------------------------------- Templates ------------------------------

misc_templates :: [(String, (String, Template, String))]
misc_templates = [
    ("predict_1.lp", ("data/misc", template_misc_1_1, "predict_1.lp")),
    ("predict_2.lp", ("data/misc", template_misc_2_1, "predict_2.lp")),
    ("predict_3.lp", ("data/misc", template_misc_3_1, "predict_3.lp")),
    ("predict_4.lp", ("data/misc", template_misc_4_1, "predict_4.lp"))
    ]

example_templates :: [Template]
example_templates = [
    template_misc_1_1, 
    template_misc_2_1,
    template_misc_3_1,
    template_misc_4_1
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
    num_causes_rules = 2
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
    num_causes_rules = 2
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
    num_causes_rules = 4
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
    num_causes_rules = 1
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
    num_causes_rules = 2
    }    

