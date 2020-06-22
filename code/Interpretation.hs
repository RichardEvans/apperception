module Interpretation where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Process as Process

-------------------------------------- Flags ----------------------------------

flag_ablation_remove_cost :: Bool
flag_ablation_remove_cost = False

flag_ablation_remove_permanents :: Bool
flag_ablation_remove_permanents = False

flag_ablation_remove_kant_condition_blind_sense :: Bool
flag_ablation_remove_kant_condition_blind_sense = False

flag_ablation_remove_kant_condition_spatial_unity :: Bool
flag_ablation_remove_kant_condition_spatial_unity = False

flag_ablation_remove_kant_condition_conceptual_unity :: Bool
flag_ablation_remove_kant_condition_conceptual_unity = False

flag_output_latex :: Bool
flag_output_latex = False

flag_condor :: Bool
flag_condor = False

flag_unicode :: Bool
flag_unicode = False

flag_delete_temp :: Bool
flag_delete_temp = False

const_time_limit :: Int
const_time_limit = 14400

-------------------------------------- Types ----------------------------------

newtype Type = T String deriving (Eq, Ord)

data Concept = C String | P String deriving (Eq, Ord)

newtype Object = O String deriving (Eq, Ord)

newtype Var = V String deriving (Eq, Ord)

data ConceptLineage = Given | Constructed deriving (Eq, Ord, Show)

data Frame = Frame {
    types :: [Type],
    type_hierarchy :: [(Type, [Type])],
    objects :: [(Object, Type)],
    exogeneous_objects :: [Object],
    permanent_concepts :: [(Concept, ConceptLineage, [Type])],
    fluid_concepts :: [(Concept, [Type])],
    input_concepts :: [Concept],
    static_concepts :: [Concept], -- concepts that don't feature in frame axiom
    vars :: [(Var, Type)],
    var_groups :: [[Var]],
    aux_files :: [String]
} deriving (Eq, Ord, Show)

data Template = Template { 
    dir :: String,
    frame :: Frame,
    min_body_atoms :: Int,
    max_body_atoms :: Int,
    num_arrow_rules :: Int,
    num_causes_rules :: Int,
    num_visual_predicates :: Maybe Int,
    use_noise :: Bool
} deriving (Eq, Ord, Show)

data GroundAtom =   GA Concept [Object] |
                    Perm Concept [Object]
                    deriving (Eq, Ord)

data VarAtom =      VA Concept [Var] |
                    Isa Concept Var |
                    Isa2 Concept Var Var 
                    deriving (Eq, Ord)

data Rule = Arrow RuleID [Atom] Atom | 
            Causes RuleID [Atom] Atom |
            Xor RuleID [Atom] [Atom]
            deriving (Eq, Ord)

type Atom = String

type RuleID = String

data InterpretationStatistics = IS {
    num_used_arrow_rules :: Int,
    num_used_causes_rules :: Int,
    total_body_atoms :: Int,
    num_inits :: Int,
    bnn_entropy :: Maybe Float,
    ambiguity :: Maybe Int,
    possible_preds :: [String]
}

data Interpretation = I {
    times :: [Int],
    senses :: [Atom],
    hiddens :: [Atom],
    exclusions :: [Atom], 
    inits :: [Atom],
    permanents :: [Atom],
    rules :: [Rule],
    facts :: [(Int, [Atom])],
    forces :: [(Int, [Atom])],
    correct :: Bool,
    num_accurate :: Maybe Int,
    num_held_outs :: Maybe Int,
    statistics :: InterpretationStatistics
}

data ClingoOutput = Answer String | Optimization String

data ClingoResult = CR {
    result_answer :: String,
    result_optimization :: String,
    result_template :: Template
}

data PredicateType = IsFluent | IsPermanent deriving (Eq, Ord)

type TypeConceptMap = Map.Map Type [Concept]

-------------------------------------------------------------------------------
-- 
-- This function generates the various ASP files from a template.
-- 
-------------------------------------------------------------------------------

do_template :: Bool -> Template -> String -> String -> IO (String, String, String)
do_template add_const t dir input_f = do
    let d = drop (length "data/") dir
    let input = take (length input_f - length ".lp") input_f
    let name = d ++ "_" ++ input
    gen_inits name t
    gen_subs name t
    gen_var_atoms name t
    gen_interpretation name t
    (script, results) <- gen_bash d input add_const t
    return (name, script, results)

gen_interpretation :: String -> Template -> IO ()
gen_interpretation name t = do
    let f = "temp/" ++ name ++ "_interpretation.lp"
    writeFile f "% Auto-generated from GenInterpretation\n\n"
    Monad.forM_ (interpretation_lines t) (append_new_line f)
    putStrLn $ "Generated " ++ f

append_new_line :: String -> String -> IO ()
append_new_line f s = appendFile f (s ++ "\n")

interpretation_lines :: Template -> [String]
interpretation_lines t = es ++ ts ++ crs ++ urs ++ cs ++ vs ++ stats where
    es = gen_elements (frame t)
    ts = gen_typing t
    (crs, n) = gen_conceptual_rules t
    urs = gen_update_rules t n
    cs = gen_constraints t
    vs = case num_visual_predicates t of
        Nothing -> []
        Just _ -> gen_visual_code t
    stats = [
        divider, "% Stats", divider, "",
        "num_objects(" ++ show (length (get_objects t)) ++ ").", 
        "num_variables(" ++ show (length (vars (frame t))) ++ ")."
        ]

gen_elements :: Frame -> [String]
gen_elements t = [divider, "% Elements", divider, ""] ++ xs where
    xs = cs ++ cs2 ++ ss ++ ts ++ [""]
    cs = map g (fluid_concepts t)
    g (x, _) = "is_concept(" ++ show x ++ ")."
    cs2 = map g2 (permanent_concepts t)
    g2 (x, _, _) = "is_concept(" ++ show x ++ ")."
    ss = map g3 (static_concepts t)
    g3 x = "is_static_concept(" ++ show x ++ ")."
    ts = map h (types t)
    h x = "is_type(" ++ show x ++ ")."

gen_typing :: Template -> [String]
gen_typing t = res where
    res = [divider, "% Typing", divider, ""] ++ xs ++ [""] ++ sts ++ [""]
    xs = map f (get_objects t)
    f (x, t) = "permanent(isa(" ++ show t ++ ", " ++ show x ++ "))."
    sts = map g (sub_types (frame t))
    g (t1, t2) = "sub_type(" ++ show t1 ++ ", " ++ show t2 ++ ")."

get_binary_concepts :: Frame -> [(Concept, [Type])] -> [(Concept, [Type])]  
get_binary_concepts frm cs = filter f cs where
    f (c, ts) = length ts == 2 && not (c `elem` input_concepts frm)

gen_unary_concepts :: Frame -> Map.Map Type [Concept]
gen_unary_concepts frm = 
    gen_unary_concepts2 frm (fluid_concepts frm) Map.empty

gen_unary_concepts2 :: Frame -> [(Concept, [Type])] -> TypeConceptMap -> TypeConceptMap     
gen_unary_concepts2 _ [] acc = acc
gen_unary_concepts2 frm ((c, [t]) : xs) acc | c `elem` input_concepts frm = gen_unary_concepts2 frm xs acc
gen_unary_concepts2 frm ((c, [t]) : xs) acc | otherwise = case Map.lookup t acc of
    Nothing -> gen_unary_concepts2 frm xs (Map.insert t [c] acc)
    Just cs -> gen_unary_concepts2 frm xs (Map.insert t (c:cs) acc)
gen_unary_concepts2 frm (_ : xs) acc = 
    gen_unary_concepts2 frm xs acc

gen_permanent_concepts :: Frame -> Map.Map Type [Concept]
gen_permanent_concepts frm = 
    gen_permanent_concepts2 frm (permanent_concepts frm) Map.empty

gen_permanent_concepts2 :: Frame -> [(Concept, ConceptLineage, [Type])] -> Map.Map Type [Concept] -> Map.Map Type [Concept]     
gen_permanent_concepts2 _ [] acc = acc
gen_permanent_concepts2 frm ((_, Given, _) : xs) acc = 
    gen_permanent_concepts2 frm xs acc
gen_permanent_concepts2 frm ((c, Constructed, [t]) : xs) acc = case Map.lookup t acc of
    Nothing -> gen_permanent_concepts2 frm xs (Map.insert t [c] acc)
    Just cs -> gen_permanent_concepts2 frm xs (Map.insert t (c:cs) acc)
gen_permanent_concepts2 frm (_ : xs) acc = 
    gen_permanent_concepts2 frm xs acc

get_permanent_constructed_concepts :: Frame -> [(Concept, [Type])]
get_permanent_constructed_concepts t = Maybe.mapMaybe f pcs where
    pcs = permanent_concepts t
    f (c, Constructed, ts) = Just (c, ts)
    f _ = Nothing

var_group_for_type :: Frame -> Type -> String
var_group_for_type frm t = 
    "var_group_" ++ drop 4 (show (var_for_type frm t))

var_for_type :: Frame -> Type -> Var
var_for_type frm t = case find_var t (vars frm) of
    Nothing -> 
        error $ "var_for_type: Type " ++ show t ++ " not found in template"
    Just v -> v

find_var :: Type -> [(Var, Type)] -> Maybe Var    
find_var t [] = Nothing
find_var t ((v, t') : _) | t == t' = Just v
find_var t (_ : vs) = find_var t vs

gen_update_rules :: Template -> Int -> [String]
gen_update_rules t n = h ++ cs ++ arrows ++ causes ++ uses ++ [""] where
    h = [divider, "% Update rules", divider, ""]
    min_bs = show (min_body_atoms t)
    max_bs = show (max_body_atoms t)
    cs = [
        "1 { rule_var_group(R, VG) : is_var_group(VG) } 1 :- is_gen_rule(R), use_rule(R).",
        "",
        min_bs ++ " { rule_body(R, VA) : is_var_atom(VA) } " ++ max_bs ++ " :- is_gen_rule(R), use_rule(R).", 
        "",
        "1 { rule_head_causes(R, VA) : cause_head(VA) } 1 :- is_causes_rule(R), use_rule(R).",
        "",
        "1 { rule_arrow_head(R, VA) : is_var_fluent(VA) } 1 :- is_arrow_rule(R), use_rule(R).",
        ""
        ]
    arrows = [ fa i | i <- [n .. n + num_arrow_rules t - 1]]
    n2 = n + num_arrow_rules t
    causes = [ fc i | i <- [n2 .. n2 + num_causes_rules t - 1]]
    fa i = "is_arrow_rule(r" ++ show i ++ ")."
    fc i = "is_causes_rule(r" ++ show i ++ ")."
    uses = ["{ use_rule(R) } :- is_arrow_rule(R).", "{ use_rule(R) } :- is_causes_rule(R)."]

divider :: String
divider = "%------------------------------------------------------------------------------"

gen_constraints :: Template -> [String]
gen_constraints t = [divider, "% Constraints", divider, ""] ++ c1 ++ c2 where
    c1 = case flag_ablation_remove_kant_condition_blind_sense of
        True -> ["% [Ignoring Kantian blind sense condition]"]
        False -> case use_noise t of
            True -> ["% Adding noise", ":~ senses(S, T), not holds(S, T). [1 @ 1, S, T]"]
            False -> [":- violation_kant_condition_blind_sense."]
    c2 = case flag_ablation_remove_kant_condition_spatial_unity of
        True -> ["% [Ignoring Kantian spatial unity condition]",
                    ""]
        False -> [":- violation_kant_condition_spatial_unity.", ""]
    c3 = case use_noise t of
        True -> ["% Adding noise", "flag_is_using_noise."]
        False -> []

gen_inits :: String -> Template -> IO ()
gen_inits name t = do
    let f = "temp/" ++ name ++ "_init.lp"
    writeFile f "% Auto-generated from GenInterpretation\n\n"
    let as = filter (\a -> not (is_static_atom t a)) (all_ground_atoms t)
    Monad.forM_ as (print_init_atom f)
    putStrLn $ "Generated " ++ f

is_static_atom :: Template -> GroundAtom -> Bool
is_static_atom t (GA c _) = c `elem` (static_concepts (frame t))
is_static_atom t (Perm c _) = c `elem` (static_concepts (frame t))

print_init_atom :: String -> GroundAtom -> IO ()
print_init_atom f a = appendFile f t where
    t = "{ " ++ show a ++ " } .\n"

gen_bash :: String -> String -> Bool -> Template -> IO (String, String)
gen_bash dir input_f add_const t = do
    let name = dir ++ "_" ++ input_f
    let task_file = "data/" ++ dir ++ "/" ++ input_f ++ ".lp"
    let d =  "temp/"
    let f = d ++ name ++ "_script.sh"
    writeFile f $ "echo \"Processing " ++ task_file ++ ".\"\n\n"
    let init_f = d ++ name ++ "_init.lp"
    let subs_f = d ++ name ++ "_subs.lp"
    let rules_f = d ++ name ++ "_var_atoms.lp"
    let interpretation_f = d ++ name ++ "_interpretation.lp"
    let auxs = map (\x -> "asp/" ++ x) (aux_files (frame t))
    let aux_s = concat (List.intersperse " " auxs)
    let results_f = d ++ name ++ "_results.txt"
    let handle = " > " ++ results_f
    let args = "--warn=no-atom-undefined --time-limit=" ++ show const_time_limit ++ " "
    let args' = if add_const then args ++ "-c k_xor_group=$1 " ++ xor_group_file_name ++ " " else args
    let clingo = if flag_condor then "/vol/lab/clingo5/clingo " else "clingo "
    let costs = if flag_ablation_remove_cost then " " else " asp/costs.lp " 
    let s = clingo ++ args' ++ task_file ++ " " ++ init_f ++ " " ++ subs_f ++ " " ++ rules_f ++ " " ++ interpretation_f ++ " " ++ aux_s ++ " " ++ " asp/judgement.lp asp/constraints.lp" ++ costs ++ handle ++ "\n\n"
    appendFile f s
    putStrLn $ "Generated " ++ f
    Process.callCommand ("chmod 777 " ++ f)
    return (f, results_f)

gen_subs :: String -> Template -> IO ()
gen_subs name t = do
    let f = "temp/" ++ name ++ "_subs.lp"
    writeFile f "% Auto-generated from GenInterpretation\n\n"
    appendFile f ("%-----------\n")
    appendFile f ("% var_types\n")
    appendFile f ("%-----------\n")
    appendFile f "\n"
    let frm = frame t
    Monad.forM_ (vars frm) $ \(v, t) -> do
        appendFile f $ "var_type(" ++ show v ++ ", " ++ show t ++ ").\n"
    appendFile f "\n\n"
    appendFile f ("%--------------\n")
    appendFile f ("% contains_var\n")
    appendFile f ("%--------------\n")
    appendFile f "\n"
    Monad.forM_ (var_groups frm) $ \vg -> do
        let n = var_group_name vg
        Monad.forM_ vg $ \v -> do
            appendFile f ("contains_var(var_group_" ++ n ++ ", " ++ show v ++ ").\n")
        appendFile f "\n"
    appendFile f "\n\n"
    appendFile f ("%----------\n")
    appendFile f ("% subs\n")
    appendFile f ("%----------\n")
    appendFile f "\n"
    Monad.forM_ (all_subs t) (print_subs_group f)
    putStrLn $ "Generated " ++ f

print_subs_group :: String -> SubsGroup -> IO () 
print_subs_group f (name, ss) = do
    Monad.forM_ (zip [1..] ss) (print_subs f name)
    appendFile f "\n"
    
print_subs :: String -> VarGroupName -> (Int, Subs) -> IO ()
print_subs f name (i, subs) = do
    appendFile f $ "subs_group(var_group_" ++ name ++ ", subs_" ++ name ++ "_" ++ show i ++ ").\n"
    Monad.forM_ subs $ \(v, x) -> do
        appendFile f $ "subs(subs_" ++ name ++ "_" ++ show i ++ ", " ++ show v ++ ", " ++ show x ++ ").\n"
    appendFile f "\n"

gen_var_atoms :: String -> Template -> IO ()
gen_var_atoms name t = do
    let f = "temp/" ++ name ++ "_var_atoms.lp"
    writeFile f "% Auto-generated from GenInterpretation\n\n"
    let frm = frame t
    Monad.forM_ (all_var_fluents frm) (print_var_fluent f frm)
    Monad.forM_ (all_var_isas frm) (print_var_isa f frm)
    putStrLn $ "Generated " ++ f

print_var_fluent :: String -> Frame -> VarAtom -> IO ()
print_var_fluent file t a = Monad.forM_ vgs f where
    f vg = appendFile file (g vg) 
    g vg = "var_fluent(" ++ show a ++ ", " ++ h vg ++ ").\n"
    vgs = all_var_groups t a 
    h vg = "var_group_" ++ var_group_name vg

print_var_isa :: String -> Frame -> VarAtom -> IO ()
print_var_isa file t a = Monad.forM_ vgs f where
    f vg = appendFile file (g vg) 
    g vg = "var_permanent(" ++ show a ++ ", " ++ h vg ++ ").\n"
    vgs = all_var_groups t a 
    h vg = "var_group_" ++ var_group_name vg

all_var_groups :: Frame -> VarAtom -> [Vars]
all_var_groups t (VA _ vs) = filter f (var_groups t) where
    f vs2 = vs `subset` vs2
all_var_groups t (Isa _ v) = filter f (var_groups t) where
    f vs2 = v `elem` vs2
all_var_groups t (Isa2 _ v v') = filter f (var_groups t) where
    f vs2 = v `elem` vs2 && v' `elem` vs2 

subset :: (Eq a) => [a] -> [a] -> Bool
subset a b = all (`elem` b) a

------------------------------- sub_types -------------------------------------

sub_types :: Frame -> [(Type, Type)]
sub_types t = concat (map f (type_hierarchy t)) where
    f (super_t, sub_ts) = [(sub_t, super_t) | sub_t <- sub_ts]

sub_types_star :: Frame -> [(Type, Type)]
sub_types_star t = fixed_point trans (sub_types t ++ [(x,x) | x <- types t])

------------------------------- frame access ----------------------------------

get_fluid_concepts :: Template -> [(Concept, [Type])]
get_fluid_concepts = fluid_concepts . frame

get_permanent_concepts :: Template -> [(Concept, ConceptLineage, [Type])]
get_permanent_concepts = permanent_concepts . frame

get_input_concepts :: Template -> [Concept]
get_input_concepts = input_concepts . frame

get_objects :: Template -> [(Object, Type)]
get_objects = objects . frame

get_exogeneous_objects :: Template -> [Object]
get_exogeneous_objects = exogeneous_objects . frame 

------------------------------- all_ground_atoms ------------------------------

all_ground_atoms :: Template -> [GroundAtom]
all_ground_atoms template = res where 
    res = ss ++ isas
    ss = concat (map f cs)    
    cs = get_fluid_concepts template
    ps = get_permanent_concepts template
    sts = sub_types_star (frame template)
    f (c, ts) = map (\xs -> GA c xs) (all_obj_tuples template ts)
    isas = isa1s ++ isa2s
    isa1s = [Perm p [x] | (p, Constructed, [t]) <- ps,
        (x, t') <- get_objects template, (t', t) `elem` sts]
    isa2s = [Perm p [x, y] | (p, Constructed, [t, t2]) <- ps,
        (x, t') <- get_objects template, (t', t) `elem` sts,
        (y, t2') <- get_objects template, (t2', t2) `elem` sts]

all_obj_tuples :: Template -> [Type] -> [[Object]]
all_obj_tuples _ [] = [[]]
all_obj_tuples template (t : ts) = res where
    res = [x : xs | x <- objs, xs <- all_obj_tuples template ts]
    objs = map fst (filter f (get_objects template))
    f (_, t') = (t', t) `elem` sts
    sts = sub_types_star (frame template)

------------------------------- all_var_atoms ---------------------------------

all_var_fluents :: Frame -> [VarAtom]
all_var_fluents t = concat (map f (fluid_concepts t)) where
    f (c, ts) = map (\xs -> VA c xs) (all_var_tuples t ts)

all_var_tuples :: Frame -> [Type] -> [[Var]]
all_var_tuples _ [] = [[]]
all_var_tuples frm (t : ts) = res where
    res = [x : xs | x <- vs, xs <- all_var_tuples frm ts]
    vs = map fst (filter f (vars frm))
    f (_, t') = (t', t) `elem` sub_types_star frm

all_var_isas :: Frame -> [VarAtom]
all_var_isas frm = as1 ++ as2 where
    as1 = [Isa c v1 | (c, _, [t1]) <- permanent_concepts frm, (v1, t1') <- vars frm, (t1', t1) `elem` sub_types_star frm]
    as2 = [Isa2 c v1 v2| (c, _, [t1, t2]) <- permanent_concepts frm, (v1, t1') <- vars frm, (t1', t1) `elem` sub_types_star frm, (v2, t2') <- vars frm, (t2', t2) `elem` sub_types_star frm]
                
------------------------------- all_subs --------------------------------------

type Subs = [(Var, Object)]
type SubsGroup = (VarGroupName, [Subs])
type VarGroupName = String
type VarTypes = [(Var, Type)]
type Vars = [Var]
type TypedVarGroup = [(Var, Type)]
type SubTypes = [(Type, Type)]
type Objects = [(Object, Type)]

all_subs :: Template -> [SubsGroup]
all_subs t = res where
    res = zip (map var_group_name (var_groups frm)) (map f vgs)
    vgs = map (make_var_group (vars frm)) (var_groups frm)
    f vars = gen2 (get_objects t) sub_types' vars
    sub_types' = fixed_point trans (sub_types frm)
    frm = frame t

make_var_group :: VarTypes -> Vars -> TypedVarGroup
make_var_group types vs = map f vs where
    f v = case lookup v types of
        Just t -> (v, t)
        Nothing -> 
            error $ "No type found for " ++ show v ++ " in " ++ show types ++ " in var group " ++ show vs

var_group_name :: Vars -> VarGroupName   
var_group_name g = res  where
    res = concat (List.intersperse "_" y)
    y = List.sort (map (f . show) g)
    f = drop 4

fixed_point :: Eq a => (a -> a) -> a -> a    
fixed_point f a = case f a == a of
    True -> a
    False -> fixed_point f (f a)

trans :: SubTypes -> SubTypes
trans xs = List.sort (List.nub rs) where
    rs = (xs ++ [(x, y) | (x, z) <- xs, (z', y) <- xs, z == z'])

gen2 :: Objects -> SubTypes -> TypedVarGroup -> [Subs]
gen2 _ _ [] = [[]]
gen2 objects sub_types ((v,t) : vs) = res where
    res = [(a:as) | a <- ps, as <- gen2 objects sub_types vs]
    ps = [(v, x) | (x, t') <- objects, is_sub_type sub_types t' t]

is_sub_type :: SubTypes -> Type -> Type -> Bool
is_sub_type _ x y | x == y = True
is_sub_type sub_types x y = (x,y) `elem` sub_types  

----------------------------- constraints -------------------------------------

group_predicates :: Ord a => [a] -> [[[a]]]
group_predicates xs = filter f (divide_into_groups xs) where
    f y = all g y
    g y = length y >= 2

divide_into_groups :: Ord a => [a] -> [[[a]]]
divide_into_groups xs = List.nub $ map f ys where
    ys = divide_into_groups2 xs [[]]
    f y = List.reverse (map g y)
    g y = List.reverse y

divide_into_groups2 :: Ord a => [a] -> [[[a]]] -> [[[a]]]
divide_into_groups2 [] acc = acc
divide_into_groups2 (x:xs) acc = a ++ b where
    a = divide_into_groups2 xs acc2
    b = divide_into_groups2 xs acc3
    acc2 = insert_in_last_list x acc
    acc3 = make_new_list x acc

insert_in_last_list :: Ord a => a -> [[[a]]] -> [[[a]]]
insert_in_last_list x gs = map f gs where
    f [] = [[x]]
    f (y:ys) = (x: y) : ys 

make_new_list :: Ord a => a -> [[[a]]] -> [[[a]]]
make_new_list x gs = map f gs where
    f [] = [[x]]
    f ys = [x] : ys

gen_xor_constraints :: Frame -> [String]
gen_xor_constraints frm = c_cs ++ c_ps where
    c_cs = concat (map (f IsFluent) cs)
    c_ps = concat (map (f IsPermanent) ps)
    cs = Map.toList (gen_unary_concepts frm)
    ps = Map.toList (gen_permanent_concepts frm)
    f k (t, cs) = gen_xor_constraints_for_type k t (List.sort cs) 

gen_xor_constraints_for_type :: PredicateType -> Type -> [Concept] -> [String]
gen_xor_constraints_for_type k t ps = r where
    r = h1 : cgs : "" : scs ++ concat (map (gen_xor_constraints_for_group k t) zs)
    h1 = "% Choose xor group"
    cgs = choose_group k t (length gs)
    gs = group_predicates ps
    zs = zip (map (group_id k t) [1..]) gs
    scs = h2 : map f ps ++ [""]
    h2 = "% Concept hierarchy"
    f p = "sub_concept(" ++ show p ++ ", " ++ show t ++ ")."

choose_group :: PredicateType -> Type -> Int -> String
choose_group k t n = "1 { " ++ m ++ " } 1." where
    m = concat (List.intersperse "; " (map (group_id k t) [1..n]))

group_id :: PredicateType -> Type -> Int -> String
group_id k t i = "xor_" ++ show k ++ "_" ++ drop 2 (show t) ++ "_" ++ show i

gen_xor_constraints_for_group :: PredicateType -> Type -> (String, [[Concept]]) -> [String]
gen_xor_constraints_for_group k t (g, pss) = r where
    r = concat (map (gen_xor_constraints_for_predicates k t g) pss)

gen_xor_constraints_for_predicates :: PredicateType -> Type -> String -> [Concept] -> [String]    
gen_xor_constraints_for_predicates k t g ps = r where
    r = at_most ++ at_least ++ incompossibles ++ output
    at_most = h1 : concat (map (gen_at_most k t g) pairs)
    incompossibles = h2 : concat (map (gen_incompossibles k t g) pairs) ++ ivs
    at_least = h3: gen_at_least k t g ps
    pairs = [(p1, p2) | p1 <- ps, p2 <- ps, p1 < p2]
    h1 = "% At most one of " ++ concat (List.intersperse ", " pss)
    h2 = "% Incompossibility " ++ concat (List.intersperse ", " pss)
    h3 = "% At least one of " ++ concat (List.intersperse ", " pss)
    pss = map show ps
    ivs = concat (map (gen_incompatible_unary_predicates k t g) pairs)
    output = gen_output g ps
    
gen_output :: String -> [Concept] -> [String]
gen_output g ps = [h, l1, l2, ""] where
    h = "% Readable exclusion" 
    l1 = "exclusion_output(\"" ++ concat (List.intersperse xor (map show ps)) ++ "\") :-"
    xor = if flag_unicode then "⊕" else "+"
    l2 = "\t" ++ g ++ "."

gen_at_most :: PredicateType -> Type -> String -> (Concept, Concept) -> [String]
gen_at_most k t g (p1, p2) = 
    [":-", "\t" ++ gen_atom k p1 ++ ", ", "\t" ++ gen_atom k p2 ++ ",", "\t" ++ g ++ ".", ""]

gen_at_least :: PredicateType -> Type -> String -> [Concept] -> [String]
gen_at_least k t g ps = [l1, l2, l3, l4] ++ nhs ++ [""] where
    l1 = ":-"
    l2 = "\tpermanent(isa(" ++ show t ++ ", X)),"
    l3 = "\tis_time(T),"
    l4 = "\t" ++ g ++ "," 
    nhs = map f (zip [1..] ls) 
    ls = map (gen_atom k) ps
    f (i, a) | i < length ps = "\tnot " ++ a ++ ","
    f (i, a) | otherwise = "\tnot " ++ a ++ "."

gen_incompatible_unary_predicates :: PredicateType -> Type -> String -> (Concept, Concept) -> [String]
gen_incompatible_unary_predicates k t g (p1, p2) = [l1, l2, l3] where
    l1 = "incompatible_unary_predicates(" ++ show p1 ++ ", " ++ show p2 ++ ") :-"
    l2 = "\t" ++ g ++ "."
    l3 = ""

gen_incompossibles :: PredicateType -> Type -> String -> (Concept, Concept) -> [String]
gen_incompossibles k t g (p1, p2) = [l1, l2, l3, l4] where
    l1 = "incompossible(" ++ gen_sentence k p1 ++ ", " ++ gen_sentence k p2 ++ ") :-"
    l2 = "\tpermanent(isa(" ++ show t ++ ", X)),"
    l3 = "\t" ++ g ++ "."
    l4 = ""

gen_atom :: PredicateType -> Concept -> String
gen_atom IsFluent p = "holds(s(" ++ show p ++ ", X), T)"
gen_atom IsPermanent p = "permanent(isa(" ++ show p ++ ", X))"

gen_atom2 :: PredicateType -> Concept -> String -> String -> String
gen_atom2 IsFluent p v1 v2 = "holds(s2(" ++ show p ++ ", " ++ v1 ++ ", " ++ v2 ++ "), T)"
gen_atom2 IsPermanent p v1 v2 = "permanent(isa2(" ++ show p ++ ", " ++ v1 ++ ", " ++ v2 ++ "))"

gen_sentence :: PredicateType -> Concept -> String
gen_sentence IsFluent p = "s(" ++ show p ++ ", X)"
gen_sentence IsPermanent p = "isa(" ++ show p ++ ", X)"

gen_sentence2 :: PredicateType -> Concept -> String -> String -> String
gen_sentence2 IsFluent p v1 v2 = "s2(" ++ show p ++ ", " ++ v1 ++ ", " ++ v2 ++ ")"
gen_sentence2 IsPermanent p v1 v2 = "isa2(" ++ show p ++ ", " ++ v1 ++ ", " ++ v2 ++ ")"

gen_exists_constraints :: Frame -> [String]
gen_exists_constraints frm = c_cs ++ c_ps where
    cs = get_binary_concepts frm (fluid_concepts frm)
    ps = get_binary_concepts frm (get_permanent_constructed_concepts frm)
    c_cs = concat (map (f IsFluent) cs)
    c_ps = concat (map (f IsPermanent) ps)
    f k (c, ts) = gen_exists_constraints_for_pred k (c, ts)

gen_exists_constraints_for_pred :: PredicateType -> (Concept, [Type]) -> [String]
gen_exists_constraints_for_pred k (c, ts) = r where
    r = sub_concept ++ at_most ++ at_least ++ incompossible
    sub_concept = [h, sc, ""]
    h = "% Concept hierarchy"
    sc = "sub_concept(" ++ show c ++ ", " ++ show (head ts) ++ ")."
    at_most = gen_exists_at_most k (c, ts)
    at_least = gen_exists_at_least k (c, ts)
    incompossible = gen_exists_incompossible k (c, ts)

gen_exists_at_most :: PredicateType -> (Concept, [Type]) -> [String]    
gen_exists_at_most k (c, _) = [h, ":-", l1, l2, l3, ""] where
    h = "% " ++ exists_string ++ "! clause for " ++ show c ++ " : at most one"
    l1 = "\t" ++ gen_atom2 k c "X" "Y" ++ ", "
    l2 = "\t" ++ gen_atom2 k c "X" "Y2" ++ ", "
    l3 = "\tY != Y2."

gen_exists_at_least :: PredicateType -> (Concept, [Type]) -> [String]    
gen_exists_at_least k (c, ts) = [h, ":-", l1, l2, l3, "", aux1, aux2, aux3, ""] where
    h = "% " ++ exists_string ++ " ! clause for " ++ show c ++ " : at least one"
    l1 = "\tpermanent(isa(" ++ show (head ts) ++ ", X)),"
    l2 = "\tis_time(T),"
    l3 = "\tnot aux_" ++ show c ++ "(X, T)."
    aux1 = "aux_" ++ show c ++ "(X, T) :-"
    aux2 = "\tis_time(T),"
    aux3 = "\t" ++ gen_atom2 k c "X" "_" ++ "."

exists_string :: String
exists_string = if flag_unicode then "∃" else "exists"

gen_exists_incompossible :: PredicateType -> (Concept, [Type]) -> [String]    
gen_exists_incompossible k (c, [t1, t2]) = [h, l1, l2, l3, l4, l5, ""] where
    h = "% Incompossibility for " ++ show c
    l1 = "incompossible(" ++ gen_sentence2 k c "X" "Y" ++ ", " ++ gen_sentence2 k c "X" "Y2" ++ ") :-"
    l2 = "\tpermanent(isa(" ++ show t1 ++ ", X)),"
    l3 = "\tpermanent(isa(" ++ show t2 ++ ", Y)),"
    l4 = "\tpermanent(isa(" ++ show t2 ++ ", Y2)),"
    l5 = "\tY != Y2."

gen_conceptual_rules :: Template -> ([String], Int)
gen_conceptual_rules t | flag_ablation_remove_kant_condition_conceptual_unity == True = (["", "% [Ignoring Kantian condition of conceptual unity]", ""], 1)
gen_conceptual_rules t | flag_ablation_remove_kant_condition_conceptual_unity == False = res where
    res = (h ++ xors ++ exists, 1)
    h = [divider, "% Conceptual structure", divider, ""] 
    xors = gen_xor_constraints frm
    exists = gen_exists_constraints frm
    frm = frame t

----------------------------- xor_group_file ----------------------------------

xor_groups :: Frame -> [[String]]
xor_groups frm = xs ++ ys where
    xs = map (f IsFluent) cs
    ys = map (f IsPermanent) ps
    cs = Map.toList (gen_unary_concepts frm)
    ps = Map.toList (gen_permanent_concepts frm)
    f k (t, cs) = xor_group k t (List.sort cs) 

xor_group :: PredicateType -> Type -> [Concept] -> [String]
xor_group k t ps = map fst zs where
    zs = zip (map (group_id k t) [1..]) gs
    gs = group_predicates ps

num_xor_groups :: Frame -> Int
num_xor_groups frm = length (sequence (xor_groups frm))

xor_groups_strings :: Frame -> [String]
xor_groups_strings frm = h ++ r where
    r = concat (map f (zip [1..] s))
    s = sequence (xor_groups frm)    
    f (i, xs) = map (g i) xs ++ [""]
    g i x = x ++ " :- xor_group(" ++ show i ++ ")."
    h = ["#const k_xor_group=1.", "", "xor_group(k_xor_group).", ""]

xor_group_file_name :: String
xor_group_file_name = "temp/gen_xor_groups.lp"

gen_xor_groups :: Template -> IO ()
gen_xor_groups t = do
    let f = xor_group_file_name
    let s = unlines (xor_groups_strings (frame t))
    writeFile f s
    putStrLn $ "Generated " ++ f

-------------------------------------------------------------------------------
-- 
-- This block of code parses clingo's results.
-- 
-------------------------------------------------------------------------------

opt = "Optimization: "

get_answers :: [String] -> [ClingoOutput] -> [ClingoOutput]
get_answers [] acc = acc
get_answers (a:b:rs) acc | List.isPrefixOf "Answer" a =
    get_answers rs (acc ++ [Answer b])
get_answers (a:rs) acc | List.isPrefixOf opt a =
    get_answers rs (acc ++ [Optimization (drop (length opt) a)])
get_answers (a:rs) acc = get_answers rs acc    

show_answer_set :: Bool
show_answer_set = False

show_extraction :: Bool
show_extraction = True

process_answer_with_template :: Template -> ClingoOutput -> String    
process_answer_with_template t (Answer l) = unlines (res1 ++ res2) where
    res1 = [h,x,h,""] ++ if show_answer_set then xs else []
    x = "Answer"
    h = "-------------"
    ws = words l
    xs = List.sort ws ++ [""]
    xs2 = filter (\x -> not ("wibble" `List.isInfixOf` x)) xs
    res2 = if show_extraction then (show_interpretation t) (extract_interpretation xs2) else []
process_answer_with_template _ (Optimization l) = "Optimization: " ++ l

show_interpretation :: Template -> Interpretation -> [String]
show_interpretation t i | flag_output_latex == True = readable_interpretation t i ++ latex_output t i
show_interpretation t i | flag_output_latex == False = readable_interpretation t i

readable_interpretation :: Template -> Interpretation -> [String]
readable_interpretation t i = is ++ ps ++ rs ++ xs ++ fs ++ ss ++ cs ++ acs where
    is = case inits i of
        [] -> []
        ins -> "" : "Initial conditions" : "------------------" : "" : ins
    ps = case permanents i of
        [] -> []
        ps -> "" : "Permanents" : "----------" : "" : ps
    rs = ["", "Rules", "-----", ""] ++ map show (rules i)
    xs = case exclusions i of
        [] -> []
        _ -> "" : "Constraints" : "-----------" : "" : exclusions i
    fs = ["", "Trace", "-----", ""] ++ map show_facts (facts i)
    cs = ["", "Accuracy", "--------", "", if correct i then "Status: correct" else "Status: incorrect"]
    acs = case (num_accurate i, num_held_outs i) of
        (Just acc, Just tot) -> let p = fromIntegral acc / fromIntegral tot :: Float in ["", "Percentage accurate: " ++ show p ++ ""]
        _ -> []
    ss = readable_stats (statistics i)

extract_interpretation :: [String] -> Interpretation
extract_interpretation xs = I { 
    times = get_times xs,
    senses = extract_senses xs,
    hiddens = extract_hiddens xs,
    exclusions = extract_exclusions xs,
    inits = extract_inits xs,
    permanents = extract_permanents xs,
    rules = extract_rules xs, 
    facts = extract_facts xs,
    forces = extract_forces xs,
    correct = extract_correct xs,
    num_accurate = extract_num_accurate xs,
    num_held_outs = extract_num_held_outs xs,
    statistics = extract_statistics xs
}

readable_stats :: InterpretationStatistics -> [String]
readable_stats i = [
    "",
    "Statistics",
    "----------",
    "",    
    "Num arrow rules: " ++ show (num_used_arrow_rules i),
    "Num causes rules: " ++ show (num_used_causes_rules i),
    "Total body atoms: " ++ show (total_body_atoms i),
    "Num inits: " ++ show (num_inits i),
    "Total cost: " ++ show (total_cost i),
    "Total num clauses: " ++ show (total_num_clauses i)
    ] ++ (case bnn_entropy i of
        Nothing -> []
        Just e -> ["Entropy of bnn : " ++ show e]) ++ 
        (case ambiguity i of
            Nothing -> [""]
            Just a -> ["Ambiguity: " ++ show a, ""]) ++
        (case possible_preds i of
            [] -> [""]
            ps -> "BNN" : "----" : "": map show_pp ps
        )

show_pp :: String -> String
show_pp p = "possible_pred(" ++ p ++ ")."

latex_stats_table :: InterpretationStatistics -> String    
latex_stats_table i = 
    show (num_used_arrow_rules i) ++ " & " ++ 
    show (num_used_causes_rules i) ++ " & " ++ 
    show (total_body_atoms i) ++ " & " ++ 
    show (num_inits i) ++ " & " ++ 
    show (total_num_clauses i) ++ " & " ++ 
    show (total_cost i) ++ " \\\\ "

total_cost :: InterpretationStatistics -> Int    
total_cost i = 
    num_used_arrow_rules i + 
    num_used_causes_rules i + 
    total_body_atoms i +
    num_inits i

total_num_clauses :: InterpretationStatistics -> Int    
total_num_clauses i = 
    num_used_arrow_rules i + 
    num_used_causes_rules i +
    num_inits i

extract_statistics :: [String] -> InterpretationStatistics
extract_statistics xs = IS {
    num_used_arrow_rules = extract_num_used_arrow_rules xs,
    num_used_causes_rules = extract_num_used_causes_rules xs,
    total_body_atoms = extract_total_body_atoms xs,
    num_inits = extract_num_inits xs + extract_num_gen_permanents xs,
    bnn_entropy = extract_bnn_entropy xs,
    ambiguity = extract_ambiguity xs,
    possible_preds = extract_possible_preds xs
    } 

extract_ambiguity :: [String] -> Maybe Int
extract_ambiguity ss = case extract_atoms "possible_pred(" ss of
    [] -> Nothing
    pps -> Just (extract_ambiguity2 pps)

extract_ambiguity2 :: [String] -> Int        
extract_ambiguity2 pps = n where
    pps2 = map (\s -> bimble_split s ',') pps
    pps3 = map f pps2
    f [x,y] = (x,y)
    m = make_map pps3
    m2 = map g (Map.toList m)
    g (_, bs) = length bs - 1
    n = sum m2

extract_possible_preds :: [String] -> [String]    
extract_possible_preds ss = extract_atoms "possible_pred(" ss

make_map :: Ord a => [(a, b)] -> Map.Map a [b]
make_map ps = List.foldl' f Map.empty ps where
    f m (a, b) = case Map.lookup a m of
        Nothing -> Map.insert a [b] m
        Just bs -> Map.insert a (b:bs) m

extract_bnn_entropy :: [String] -> Maybe Float
extract_bnn_entropy ss = 
    case (bnn_es, possible_preds) of
        ([], _) -> Nothing
        (cs, pps) -> Just (calculate_entropy cs pps num_bvs)
    where
        bnn_es = extract_atoms "count_bnn_examples_per_predicate(" ss
        possible_preds = extract_atoms "is_possible_pred(" ss
        num_bvs = extract_atoms "num_bvs(" ss

calculate_entropy :: [String] -> [String] -> [String] -> Float        
calculate_entropy ss pps num_bvs_string= e where
    num_bvs = read (head num_bvs_string) :: Int
    ss' = map (\s -> bimble_split s ',') ss
    ns = map (\x -> x !! 1) ss'
    n = map read ns :: [Int]
    fs = map fromIntegral n :: [Float]
    tot = fromIntegral num_bvs :: Float
    dist = map ( / tot) fs
    b = fromIntegral (length pps) :: Float
    xs = map (\p -> if p <= 0.0 then 0.0 else - p * logBase b p) dist
    e = if b <= 1 then 0.0 else sum xs :: Float

extract_num_used_arrow_rules xs = length (extract_atoms "used_arrow_rule(" xs)
extract_num_used_causes_rules xs = length (extract_atoms "used_causes_rule(" xs)
extract_total_body_atoms xs = length (extract_atoms "rule_body(" xs)
extract_num_inits xs = length (extract_atoms "init(" xs)
extract_num_gen_permanents xs = length (extract_atoms "gen_permanent(" xs)

extract_correct :: [String] -> Bool
extract_correct = any f where
    f = List.isPrefixOf "correct"

extract_exclusions :: [String] -> [Atom]
extract_exclusions xs = Maybe.mapMaybe f xs where
    p = "exclusion_output(\""
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ drop_last (drop_last (List.drop (length p) x))

extract_num_accurate = extract_maybe_int "count_num_accurate("
extract_num_held_outs = extract_maybe_int "count_num_held_out_time_steps("

extract_maybe_int :: String -> [String] -> Maybe Int
extract_maybe_int p xs = case extract_atoms p xs of
    [] -> Nothing
    [x] -> Just (read x)
    _ -> error "Unexpected multiple strings matching pattern"

extract_times = extract_atoms "is_time("
extract_senses = extract_atoms "senses("
extract_hiddens = extract_atoms "hidden("

extract_atoms :: String -> [String] -> [Atom]
extract_atoms p xs = Maybe.mapMaybe f xs where
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ drop_last (List.drop (length p) x)

extract_inits :: [String] -> [Atom]
extract_inits xs = Maybe.mapMaybe f xs where
    p = "init("
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ drop_last (List.drop (length p) x)

extract_permanents :: [String] -> [Atom]
extract_permanents xs = Maybe.mapMaybe f xs where
    p = "gen_permanent("
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ drop_last (List.drop (length p) x)

extract_rules :: [String] -> [Rule]
extract_rules xs = extract_xors xs ++ extract_arrows xs ++ extract_causes xs

extract_xors :: [String] -> [Rule]
extract_xors xs = Maybe.mapMaybe f (extract_xor_heads xs) where
    f (r, hs) | List.isPrefixOf "r_input" r = Nothing
    f (r, hs) | otherwise = Just $ Xor r (extract_body xs r) hs

extract_arrows :: [String] -> [Rule]
extract_arrows xs = map f (extract_arrow_heads xs) where
    f (r, c) = Arrow r (extract_body xs r) c

extract_causes :: [String] -> [Rule]
extract_causes xs = map f (extract_cause_heads xs) where
    f (r, c) = Causes r (extract_body xs r) c

extract_xor_heads :: [String] -> [(RuleID, [Atom])]
extract_xor_heads xs = collect_pairs ps where
    ps = Maybe.mapMaybe f xs
    p = "rule_head_xor("
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ 
            extract_arrow_pair (drop_last (List.drop (length p) x))

collect_pairs :: Eq a => [(a, b)] -> [(a, [b])]
collect_pairs xs = map f as where
    as = List.nub (map fst xs)
    f a = (a, g a)
    g a = [b | (a', b) <- xs, a' == a]

extract_arrow_heads :: [String] -> [(RuleID, Atom)]
extract_arrow_heads xs = Maybe.mapMaybe f xs where
    p = "rule_arrow_head("
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ 
            extract_arrow_pair (drop_last (List.drop (length p) x))

bimble_split :: String -> Char -> [String]
bimble_split s c = bimble_split2 s c ""

bimble_split2 :: String -> Char -> String -> [String]
bimble_split2 "" _  acc = [acc]
bimble_split2 (x:xs) c acc | x == c = acc : bimble_split2 xs c ""
bimble_split2 (x:xs) c acc | otherwise = bimble_split2 xs c (acc ++ [x])

extract_arrow_pair :: String -> (RuleID, Atom)
extract_arrow_pair x = (r, xs2) where
    r:xs = bimble_split x ','
    xs2 = concat (List.intersperse ", " xs)

extract_cause_heads :: [String] -> [(RuleID, Atom)]
extract_cause_heads xs = Maybe.mapMaybe f xs where
    p = "rule_head_causes("
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ 
            extract_cause_pair (drop_last (List.drop (length p) x))

extract_body :: [String] -> RuleID -> [Atom]
extract_body xs r = Maybe.mapMaybe f xs where
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ drop_last (List.drop (length p) x)
    p = "rule_body(" ++ r ++ ","

extract_cause_pair :: String -> (RuleID, Atom)
extract_cause_pair x = (r, xs2) where
    r:xs = bimble_split x ','
    xs2 = concat (List.intersperse ", " xs)

drop_last :: [a] -> [a]        
drop_last x = reverse (List.drop 1 (reverse x))

extract_facts :: [String] -> [(Int, [Atom])]
extract_facts = bring_together . (extract_pred "holds(")

extract_forces :: [String] -> [(Int, [Atom])]
extract_forces = bring_together . (extract_pred "force(")

extract_pred :: String -> [String] -> [(Int, Atom)]
extract_pred p xs = Maybe.mapMaybe f xs where
    f x = case List.isPrefixOf p x of
        False -> Nothing
        True -> Just $ 
            extract_holds_pair (drop_last (List.drop (length p) x))

extract_holds_pair :: String -> (Int, Atom)            
extract_holds_pair x = (t, s) where
    xs = bimble_split x ','
    t_s = head (reverse xs)
    t = read t_s :: Int
    s = concat (List.intersperse ", " (drop_last xs))

bring_together :: [(Int, Atom)] -> [(Int, [Atom])]
bring_together xs = map f ns where
    ns = setify (map fst xs)
    f n = (n, Maybe.mapMaybe (g n) xs)
    g n (n', x) | n == n' = Just x
    g n (n', x) | otherwise = Nothing

setify :: Ord a => [a] -> [a]        
setify = Set.toList . Set.fromList

show_facts :: (Int, [Atom]) -> String
show_facts (t, fs) = unlines (x : xs) where
    x = "Time " ++ show t ++ ": "
    xs = fs

and_string :: String
and_string = if flag_unicode then " ∧ " else " /\\ "

arrow_string :: String 
arrow_string = if flag_unicode then " → " else " -> "

causes_string :: String
causes_string = if flag_unicode then " ▸ " else " >> "

xor_string :: String
xor_string = if flag_unicode then " ⊕ " else " + " 

instance Show Rule where
    show (Arrow r bs h) = r ++ " : " ++ concat (List.intersperse and_string bs) ++ arrow_string ++ h
    show (Causes r bs c) = r ++ " : " ++ concat (List.intersperse and_string bs) ++ causes_string ++ c
    show (Xor r bs hs) = r ++ " : " ++ concat (List.intersperse and_string bs) ++ arrow_string ++ concat (List.intersperse xor_string hs)

-- We only want to display the optimum answer, not the earlier ones.
last_answers :: [ClingoOutput] -> [ClingoOutput]
last_answers ls = case last ls of
    Answer s -> [last ls]
    Optimization s -> [ls !! (length ls - 2), last ls]

-------------------------------------------------------------------------------
-- 
-- Generating LATEX-readable output.
-- 
-------------------------------------------------------------------------------

write_latex :: Template -> ClingoOutput -> IO ()
write_latex _ _ | flag_output_latex == False = return ()
write_latex t (Optimization _) = return ()    
write_latex t (Answer l) = do
    let ws = words l
    let xs = List.sort ws ++ [""]
    let xs2 = filter (\x -> not ("wibble" `List.isInfixOf` x)) xs
    let i = extract_interpretation xs2
    let x = latex_output t i
    let file = "temp/results.tex"
    putStrLn $ "Creating latex file " ++ file
    writeFile file (unlines x)

latex_output :: Template -> Interpretation -> [String]
latex_output t i = latex_header ++ latex_given i ++ latex_frame t ++ latex_interpretation t i ++ latex_footer

latex_header :: [String]
latex_header = ["", "\\begin{example}"]

latex_footer :: [String]
latex_footer = ["\\end{example}", ""]

data InputTableElem = ITE {
    time :: Int,
    object :: String,
    attribute :: String,
    hidden :: Bool
} deriving (Eq, Ord, Show)

data InputTable = IT {
    max_time :: Int,
    all_objects :: [String],
    attributes :: Map.Map (Int, String) String
}

input_table :: [Int] -> [InputTableElem] -> InputTable
input_table ts elems = IT { max_time = maximum ts, all_objects = xs, attributes = as } where
    xs = Set.toList (Set.fromList (map object elems))
    as = List.foldl' f Map.empty elems
    f m ite | hidden ite == False = Map.insert (time ite, object ite) (attribute ite) m
    f m ite | otherwise = m
    
show_input_table :: InputTable -> [String]
show_input_table table = header ++ map f times ++ footer where
    header = ["\\begin{table}[H]", "\\begin{center}", "\\begin{tabular}{|l|" ++ fsx ++ "}", "\\hline", "time & " ++ objs_text ++ "\\\\", "\\hline"]
    objs_text = concat (List.intersperse "& " (all_objects_text table))
    objs = all_objects table
    num_objects = length objs
    fsx = concat (replicate num_objects "r|")
    f t = show t ++ " & " ++ concat (List.intersperse "& " (map (g t) objs)) ++ " \\\\"
    g t obj = case Map.lookup (t, obj) (attributes table) of
        Nothing -> "?"
        Just a -> convert_token a
    footer = ["\\hline", "\\end{tabular}", "\\end{center}", "\\end{table}"]
    times = [1 .. max_time table]

all_objects_text :: InputTable -> [String]     
all_objects_text t = map f (all_objects t) where
    f = g . convert_to_mathit
    g x = "$" ++ x ++ "$"

convert_token :: String -> String
convert_token x = "$" ++ convert_to_mathit x ++ "$"

get_times :: [String] -> [Int]
get_times ws = map read (extract_times ws)

show_attribute :: InputTableElem -> String    
show_attribute ite | hidden ite == False = "$" ++ convert_to_mathit (attribute ite) ++ "$"
show_attribute ite | hidden ite == True = "?"

latex_given :: Interpretation -> [String]
latex_given i = header ++ show_input_table t where
    header = ["", "Given the following input:"]
    f b = extract_input_table_elem b . extract_time_pair . replace_atom
    ss = map (f False) (senses i)
    hs = map (f True) (hiddens i)
    t = input_table (times i) (ss ++ hs)

extract_time_pair :: String -> (Int, String)
extract_time_pair s = (t, s') where
    comma_indices = List.findIndices (== ',') s
    i = last comma_indices
    s' = take i s
    t = read (drop (i+1) s) :: Int

extract_input_table_elem :: Bool -> (Int, String) -> InputTableElem
extract_input_table_elem h (t, s) = ITE { time = t, object = x, attribute = y, hidden = h } where (x,y) = extract_object_attribute s

extract_object_attribute :: String -> (String, String)
extract_object_attribute s = f ws where
    ws = split_one_of "()," s
    f ("s2":_:obj:attr:_) = (obj, attr)
    f ("s":attr:obj:_) = (obj, attr)
    f others = error $ "Unexpected arg: " ++ show others

split_one_of :: String -> String -> [String]    
split_one_of x s = words_p (`elem` x) s

words_p :: (Char -> Bool) -> String -> [String]
words_p p s =  case dropWhile p s of
    "" -> []
    s' -> w : words_p p s'' where (w, s'') = break p s'

latex_interpretation :: Template -> Interpretation -> [String]
latex_interpretation t i = ht ++ it ++ urt ++ cs ++ et where
    ht = ["Our system finds the theory $\\theta = (\\phi, I, R, C)$, where:", 
            "\\begin{eqnarray*}"]
    it = latex_initial_conditions i
    urt = latex_update_rules i
    cs = latex_conditions t i
    et = ["\\end{eqnarray*}", ""]

latex_objects :: Template -> [String]
latex_objects tmp = h : os ++ [t] where
    h = "O & = & \\left\\{ \\begin{array}{l}"
    objs = [(x, y) | (x, y) <- get_objects tmp, not (x `elem` get_exogeneous_objects tmp)]
    os = map f objs
    f (O o, T t) = convert_to_mathit o ++ ": " ++ convert_to_mathit t ++ "\\\\"
    t = "\\end{array}\\right\\}\\\\"

latex_initial_conditions :: Interpretation -> [String]
latex_initial_conditions i = h : os ++ [t] where
    h = "I & = & \\left\\{ \\begin{array}{l}"
    os = map f (inits i ++ permanents i)
    f a = convert_atom a ++ "\\\\"
    t = "\\end{array}\\right\\}\\\\"

latex_conditions :: Template -> Interpretation -> [String]
latex_conditions tmp i = h : xs ++ ys ++ [t] where
    h = "C & = & \\left\\{ \\begin{array}{l}"
    xs = map f (exclusions i)
    f a = convert_condition tmp a ++ "\\\\"
    t = "\\end{array}\\right\\}\\\\"
    ys = map g (binary_fluents (frame tmp))
    g (f, T t1, T t2) = "\\forall X : " ++ t1 ++ ", \\; \\exists ! Y : " ++ t2 ++ ", \\; " ++ f ++ "(X, Y) \\\\"

binary_fluents :: Frame -> [(String, Type, Type)]
binary_fluents x = Maybe.mapMaybe f (fluid_concepts x) where
    f (C p, [t1, t2]) = Just (p, t1, t2)
    f _ = Nothing

latex_update_rules :: Interpretation -> [String]
latex_update_rules i = h : rs ++ [t] where
    h = "R & = &  \\left\\{ \\begin{array}{l}"
    rs = map latex_rule (List.sort (rules i))
    t = "\\end{array}\\right\\}\\\\"

latex_rule :: Rule -> String
latex_rule (Arrow r bs h) = t where
    t = bst ++ " \\rightarrow " ++ ht ++ "\\\\"
    bst = concat (List.intersperse " \\wedge " bs2)
    bs2 = map convert_atom bs
    ht = convert_atom h
latex_rule (Causes r bs h2) = t where
    t = bst ++ " \\fork " ++ ht2 ++ "\\\\"
    bst = concat (List.intersperse " \\wedge " bs2)
    bs2 = map convert_atom bs
    ht2 = convert_atom h2

sep2 :: String -> String -> [String]
sep2 s1 s2 = words $ map(\x -> if (x `elem` s1) then ' ' else x) s2

convert_condition :: Template -> String -> String
convert_condition tmp s = "\\forall X : " ++ t ++ ",\\;" ++ y where
    y = concat (List.intersperse " \\oplus " xs2)
    xs = sep2 "+" s2
    s2 = replace_atom s
    xs2 = map f xs
    f x = convert_to_mathit (x ++ "(X)")
    t = get_unary_predicate_type (frame tmp) (head xs)

get_unary_predicate_type :: Frame -> String -> String
get_unary_predicate_type f s = case lookup (C s) (fluid_concepts f) of
    Just [T t] -> t
    Nothing -> case lookup (P s) (conv2 (permanent_concepts f)) of
        Just (_, [T t]) -> t
        Nothing -> error $ "Failed to find: " ++ s

conv2 :: [(x, y, z)] -> [(x, (y, z))]
conv2 = map f where
    f (x, y, z) = (x, (y, z))

convert_atom :: String -> String
convert_atom = de_reify_atom . replace_atom

replace_atom :: String -> String
replace_atom s = (Text.unpack t') where
    t = Text.pack s :: Text.Text
    r = Text.pack "" :: Text.Text
    s1 = Text.pack "c_" :: Text.Text
    s2 = Text.pack "p_" :: Text.Text
    s3 = Text.pack "obj_" :: Text.Text
    t' = Text.replace s3 r (Text.replace s2 r (Text.replace s1 r t))

de_reify_atom :: String -> String
de_reify_atom s = convert_to_mathit x ++ "(" ++ ys ++ ")" where
    (x:xs) = drop 1 (sep2 "(,)" s)
    xs2 = map convert_term xs
    ys = concat (List.intersperse "," xs2)

convert_term :: String -> String
convert_term = convert_to_mathit . convert_var

convert_to_mathit :: String -> String
convert_to_mathit s = "\\mathit{" ++ x ++ "}" ++ y where
    (x, y) = split_alpha s

split_alpha :: String -> (String, String)
split_alpha s = case List.findIndex (not . Char.isAlpha) s of
    Just i -> (take i s, drop i s)
    Nothing -> (s, "")

convert_var :: String -> String
convert_var s | List.isPrefixOf " var_" s = Text.unpack $ Text.toUpper (Text.pack $ drop 5 s)
convert_var s | List.isPrefixOf "var_" s = Text.unpack $ Text.toUpper (Text.pack $ drop 4 s)
convert_var s | otherwise = s

latex_frame :: Template -> [String]
latex_frame t = ht ++ tst ++ ot ++ cst ++ vs ++ et where
    f = frame t
    ht = ["\\noindent", "Our system produces the frame $\\phi = (T, O, P, V)$, where:", "\\begin{eqnarray*}"]
    tst = latex_types f
    ot = latex_objects t
    cst = latex_concepts f
    vs = latex_vars f
    et = ["\\end{eqnarray*}"]

latex_types :: Frame -> [String]
latex_types frm = h : os ++ [t] where
    h = "T & = & \\left\\{ \\begin{array}{l}"
    os = map f (List.sort (types frm))
    f (T t) = convert_to_mathit t ++ "\\\\"
    t = "\\end{array}\\right\\}\\\\"

latex_sub_types :: Frame -> [String]
--latex_sub_types frm | null (sub_types frm) = ["\\sqsubseteq & = & \\{\\}\\\\"]
latex_sub_types frm = h : os ++ [t] where
    h = "\\sqsubseteq & = & \\left\\{ \\begin{array}{l}"
    os = map f (List.sort (sub_types frm))
    f (T t1, T t2) = convert_to_mathit t1 ++ " \\sqsubseteq " ++ convert_to_mathit t2 ++ "\\\\"
    t = "\\end{array}\\right\\}\\\\"

latex_concepts :: Frame -> [String]
latex_concepts frm = h : os ++ [t] where
    h = "P & = & \\left\\{ \\begin{array}{l}"
    os = map f (get_all_concepts frm)
    f (C c, ts) = convert_to_mathit c ++ "(" ++ concat (List.intersperse ", " (map g ts)) ++ ")\\\\"
    f (P c, ts) = convert_to_mathit c ++ "(" ++ concat (List.intersperse ", " (map g ts)) ++ ")\\\\"
    g (T t) = convert_to_mathit t
    t = "\\end{array}\\right\\}\\\\"

get_all_concepts :: Frame -> [(Concept, [Type])]
get_all_concepts frm = List.sort (fs ++ ps) where
    fs = fluid_concepts frm
    ps = Maybe.mapMaybe f (permanent_concepts frm)
    f (c, Constructed, ts) = Just (c, ts)
    f (_, Given, _) = Nothing

latex_vars :: Frame -> [String]
latex_vars frm = h : os ++ [t] where
    h = "V & = & \\left\\{ \\begin{array}{l}"
    os = map f (vars frm)
    f (v, T t) = convert_var (show v) ++ ": " ++ convert_to_mathit t ++ "\\\\"
    t = "\\end{array}\\right\\}\\\\"

latex_var_types :: Frame -> [String]
latex_var_types frm = h : os ++ [t] where
    h = "\\kappa_V & = & \\left\\{ \\begin{array}{l}"
    os = map f (vars frm)
    f (v, T t) = convert_var (show v) ++ " \\mapsto " ++ t ++ "\\\\"
    t = "\\end{array}\\right\\}\\\\"

-------------------------------------- Show -----------------------------------

instance Show Type where
    show (T x) = "t_" ++ x

instance Show Concept where
    show (C x) = "c_" ++ x
    show (P x) = "p_" ++ x

instance Show Object where
    show (O x) = "obj_" ++ x

instance Show Var where
    show (V x) = "var_" ++ x

instance Show GroundAtom where
    show (GA c [x]) = "init(s(" ++ show c ++ ", " ++ show x ++ "))"
    show (GA c [x, x2]) = "init(s2(" ++ show c ++ ", " ++ show x ++ ", " ++ show x2 ++ "))"
    show (Perm t [x]) = "gen_permanent(isa(" ++ show t ++ ", " ++ show x ++ "))"
    show (Perm t [x, x2]) = "gen_permanent(isa2(" ++ show t ++ ", " ++ show x ++ ", " ++ show x2 ++ "))"

instance Show VarAtom where
    show (VA c [x]) = "s(" ++ show c ++ ", " ++ show x ++ ")"
    show (VA c [x, x2]) = "s2(" ++ show c ++ ", " ++ show x ++ ", " ++ show x2 ++ ")"
    show (Isa t x) = "isa(" ++ show t ++ ", " ++ show x ++ ")"
    show (Isa2 t x y) = "isa2(" ++ show t ++ ", " ++ show x ++  ", " ++ show y ++ ")"

instance Show PredicateType where
    show IsFluent = "fluent"
    show IsPermanent = "permanent"

-------------------------------------------------------------------------------
-- 
-- Generating ILASP code.
-- 
-------------------------------------------------------------------------------

write_ilasp :: String -> String -> Int -> Template -> Int -> IO ()
write_ilasp d input maxv t recall = do
    let dir = "data_" ++ d
    xs <- readFile (dir ++ "/" ++ input)
    ys <- readFile "asp/ilasp.lp"
    let f = "ILASP/" ++ d ++ "_" ++ input ++ ".las"
    let ls = ilasp_file dir input maxv t recall ++ lines xs ++ lines ys
    putStrLn $ "Generating file " ++ f
    writeFile f (unlines ls) 

ilasp_file :: String -> String -> Int -> Template -> Int -> [String]
ilasp_file dir input maxv t recall = ilasp_lines maxv t recall ++ ilasp_xors t ++ ilasp_type_info t

ilasp_xors :: Template -> [String]
ilasp_xors t = xors ++ exists where
    xors = gen_xor_constraints frm
    exists = gen_exists_constraints frm
    frm = frame t

ilasp_type_info :: Template -> [String]
ilasp_type_info t = map f (all_object_types t) where
    f (t, x) = "permanent(isa(" ++ show t ++ ", " ++ show x ++ "))."

ilasp_lines :: Int -> Template -> Int -> [String]
ilasp_lines maxv t n = ss where
    ss = bs ++ s ++ ilasp_inits t ++ ilasp_rules frm n ++ ilasp_constants t    
    -- b_on_off is special-cased for ECA
    -- TODO: generalize!
    b_on_off = "#bias(\":- body(holds(s(c_on, X), T)), body(holds(s(c_off, X), T)).\")."
    s = b_on_off : [
        "#bias(\":- body(holds(_, T1)), body(holds(_, T2)), T1 < T2.\").", 
        "#maxv(" ++ show maxv ++ ").", 
        "#pos(a, {}, {})."
        ]
    frm = frame t
    bs = biases t

biases :: Template -> [String]
biases t = fluent_relation_biases t ++ permanent_relation_biases t    

fluent_relation_biases :: Template -> [String]
fluent_relation_biases t = map f fs' where
    fs = fluid_concepts (frame t)
    fs' = filter is_relation fs
    is_relation (_, [_, _]) = True
    is_relation _ = False
    f (c, _) = "#bias(\":- body(holds(s2(" ++ show c ++ ", X, Y), T)), body(holds(s2(" ++ show c ++ ", X, Y2), T)), Y > Y2.\")."
    
permanent_relation_biases :: Template -> [String]
permanent_relation_biases t = map f fs' where
    fs = permanent_concepts (frame t)
    fs' = filter is_relation fs
    is_relation (_, _, [_, _]) = True
    is_relation _ = False
    f (c, _, _) = "#bias(\":- body(permanent(isa2(" ++ show c ++ ", X, Y))), body(permanent(isa2(" ++ show c ++ ", X, Y2))), Y > Y2.\")."

ilasp_constants :: Template -> [String]
ilasp_constants template = time : types ++ consts where
    time = "t_time(X) :- is_time(X)."
    types = [show t ++ "(" ++ show x ++ ")." | (t, x) <- all_object_types template]
    consts = ["#constant(" ++ show t ++ ", " ++ show x ++ ")." | (t, x) <- all_object_types template]

all_object_types :: Template -> [(Type, Object)]
all_object_types template = [(t2, x) | 
    (x, t) <- get_objects template,
    (t', t2) <- sub_types_star (frame template),
    t == t']

ilasp_rules :: Frame -> Int -> [String]
ilasp_rules frm n = ilasp_rule_heads frm ++ ilasp_rule_bodies frm n

ilasp_rule_heads :: Frame -> [String]
ilasp_rule_heads frm = ilasp_causes frm ++ ilasp_arrows frm

ilasp_causes :: Frame -> [String]
ilasp_causes frm = cause_unary frm ++ cause_binary frm 

cause_unary :: Frame -> [String]
cause_unary frm = map f (unary_fluent_concept_pairs frm) where
    f (c1, c2, t) = "#modeh(causes(s(" ++ show c1 ++ ", var(" ++ show t ++ ")), s(" ++ show c2 ++ ", var(" ++ show t ++ ")), var(t_time)))."

unary_fluent_concept_pairs :: Frame -> [(Concept, Concept, Type)] 
unary_fluent_concept_pairs frm = [(c1, c2, t) | 
    (c1, [t]) <- fluid_concepts frm,
    (c2, [t']) <- fluid_concepts frm,
    c1 /= c2,
    t == t'
    ]

cause_binary :: Frame -> [String]
cause_binary frm = map f bcs where
    bcs = filter g (fluid_concepts frm)
    g (_, [_, _]) = True
    g _ = False
    f (c, [t1, t2]) = "#modeh(causes(s2(" ++ show c ++ ", var(" ++ show t1 ++ "), var(" ++ show t2 ++ ")), s2(" ++ show c ++ ", var(" ++ show t1 ++ "), var(" ++ show t2 ++ ")), var(t_time)))."

ilasp_arrows :: Frame -> [String]
ilasp_arrows frm = map f (ilasp_fluent_atoms frm) where
    f s = "#modeh(arrow(" ++ s ++ ", var(t_time)))."

fluid_sigs :: Frame -> Map.Map [Type] [Concept]
fluid_sigs frm = List.foldl' f Map.empty (fluid_concepts frm) where
    f b (p, ts) = case Map.lookup ts b of
        Nothing -> Map.insert ts [p] b
        Just ps -> Map.insert ts (p:ps) b

permanent_sigs :: Frame -> Map.Map [Type] [Concept]
permanent_sigs frm = List.foldl' f Map.empty (permanent_concepts frm) where
    f b (p, _, ts) = case Map.lookup ts b of
        Nothing -> Map.insert ts [p] b
        Just ps -> Map.insert ts (p:ps) b

ilasp_rule_bodies :: Frame -> Int -> [String]
ilasp_rule_bodies frm n = xs ++ ys where
    xs = concat (map f zsigs)
    zsigs = zip [1..] (Map.assocs (fluid_sigs frm))
    f (i, (ts, ps)) = mode_b_fluid i ts : map (g "fluid" i) ps
    g s i p = "#constant(t_pred_" ++ s ++ "_" ++ show i ++ ", " ++ show p ++ ")."
    ys = concat (map f2 zsigs2)
    zsigs2 = zip [1..] (Map.assocs (permanent_sigs frm))
    f2 (i, (ts, ps)) = mode_b_permanent i ts : map (g "permanent" i) ps
    mode_b_fluid i [t] = "#modeb(" ++ show n ++ ", holds(s(const(t_pred_fluid_" ++ show i ++ "), var(" ++ show t ++ ")), var(t_time)), (positive))."
    mode_b_fluid i [t, t2] = "#modeb(" ++ show n ++ ", holds(s2(const(t_pred_fluid_" ++ show i ++ "), var(" ++ show t ++ "), var(" ++ show t2 ++ ")), var(t_time)), (positive))."
    mode_b_permanent i [t] = "#modeb(" ++ show n ++ ", permanent(isa(const(t_pred_permanent_" ++ show i ++ "), var(" ++ show t ++ "))), (positive))."
    mode_b_permanent i [t, t2] = "#modeb(" ++ show n ++ ", permanent(isa2(const(t_pred_permanent_" ++ show i ++ "), var(" ++ show t ++ "), var(" ++ show t2 ++ "))), (positive))."

-- These body atoms are generated so that rules can have 
-- empty bodies but still be safe.
--
-- With ILASP 3.3, this is no longer necessary if we use --strict-types.
ilasp_safe_body_atoms :: Frame -> [String]
ilasp_safe_body_atoms frm = x : xs where
    x = "#modeb(1, is_time(var(t_time)), (positive))."
    xs = map f ts
    f t = "#modeb(1, is_object(var(" ++ show t ++ ")), (positive))."
    ts = all_sub_types_of frm (T "object")

all_sub_types_of :: Frame -> Type -> [Type]
all_sub_types_of frm t = [x | (x, t') <- sub_types_star frm, t' == t]

ilasp_all_atoms :: Frame -> [String]
ilasp_all_atoms frm = ilasp_fluent_atoms frm ++ ilasp_permanent_atoms frm

ilasp_fluent_atoms :: Frame -> [String]
ilasp_fluent_atoms frm = map f (fluid_concepts frm) where
    f (c, [t]) = "holds(s(" ++ show c ++ ", var(" ++ show t ++ ")), var(t_time))"
    f (c, [t1, t2]) = "holds(s2(" ++ show c ++ ", var(" ++ show t1 ++ "), var(" ++ show t2 ++ ")), var(t_time))"
    f _ = error "Unsupported predicates with arity > 2"

ilasp_permanent_atoms :: Frame -> [String]
ilasp_permanent_atoms frm = map f (permanent_concepts frm) where
    f (c, _, [t]) = "isa(" ++ show c ++ ", var(" ++ show t ++ "))"
    f (c, _, [t1, t2]) = "isa2(" ++ show c ++ ", var(" ++ show t1 ++ "), var(" ++ show t2 ++ "))"
    f _ = error "Unsupported predicates with arity > 2"

ilasp_inits :: Template -> [String]
ilasp_inits t = ilasp_initial_fluents t ++ ilasp_initial_permanents t

ilasp_initial_fluents :: Template -> [String]
ilasp_initial_fluents template = concat (map f (fluid_concepts frm)) where
    frm = frame template
    f (c, [t]) = ["1 ~ init(s(" ++ show c ++ ", " ++ show x ++ "))." | x <- all_objects_of_type template t]
    f (c, [t1, t2]) = ["1 ~ init(s2(" ++ show c ++ ", " ++ show x1 ++ ", " ++ show x2 ++ "))." | x1 <- all_objects_of_type template t1, x2 <- all_objects_of_type template t2]
    f _ = error "Unsupported predicates with arity > 2"

ilasp_initial_permanents :: Template -> [String]
ilasp_initial_permanents template = concat (map f (permanent_concepts frm)) where
    frm = frame template
    f (c, Given, _) = []
    f (c, Constructed, [t]) = ["1 ~ gen_permanent(isa(" ++ show c ++ ", " ++ show x ++ "))." | x <- all_objects_of_type template t]
    f (c, Constructed, [t1, t2]) = ["1 ~ gen_permanent(isa2(" ++ show c ++ ", " ++ show x1 ++ ", " ++ show x2 ++ "))." | x1 <- all_objects_of_type template t1, x2 <- all_objects_of_type template t2]
    f _ = error "Unsupported predicates with arity > 2"
     
all_objects_of_type :: Template -> Type -> [Object]
all_objects_of_type template t = Maybe.mapMaybe f (get_objects template) where
    f (x, t') = case (t', t) `elem` sub_types_star (frame template) of
        True -> Just x
        False -> Nothing

-------------------------------------------------------------------------------
-- 
-- Unary predicate assignments
-- 
-- Given a set P = {P1, ..., Pn} of unary predicates,
-- and a set X = {x1, ..., xm} of objects,
-- generate all total mappings m : X → P such that...
--  (i) surjection: for all p in P, there is some x in X such that m(x) = p
--  (ii) ordering: if m(x_i) = i', m(x_j) = j' and i <= j then i' <= j'
--  (iii) if i <= j then |{x | m(x) = p_i}| <= |{x | m(x) = p_j}|
-------------------------------------------------------------------------------
all_unary_predicate_assignments :: Int -> Int -> [[Int]]
all_unary_predicate_assignments num_preds num_objs | num_preds > num_objs = error "This function not well formed when num_preds exceeds num_objs"
all_unary_predicate_assignments num_preds num_objs = filter f ls where
    ls = all_lists_from_1_of_length_2 [1..num_preds] num_objs
    f l = cond1 l && cond2 l && cond3 l
    cond1 l = all (g l) [1 .. num_preds]
    g l p = p `elem` l
    cond2 l = all (h l) ps
    ps = [(i, j) | i <- [0 .. num_objs-1], j <- [0 .. num_objs-1], i <= j]
    h l (i, j) = l !! i <= l !! j
    cond3 l = all (check_len l) [1 .. num_preds - 1]
    check_len l i = count_ps l i <= count_ps l (i+1)
    count_ps l p = length [i | i <- [0 .. length l - 1], l !! i == p]

all_lists_from_1_of_length_2 :: [a] -> Int -> [[a]]    
all_lists_from_1_of_length_2 xs 0 = [[]]
all_lists_from_1_of_length_2 xs n = ls where
    ls = [(x : ls') | x <- xs, ls' <- all_lists_from_1_of_length_2 xs (n-1)]

-- Given a set of objects and a number of visual predicates,
-- generate the code for each mapping, 
-- plus the choice rule to decide between the mappings.
gen_visual_mappings :: [String] -> Int -> [String]
gen_visual_mappings objs num_preds = choice_rule ++ concat (map f zms) where
    ms = all_unary_predicate_assignments num_preds (length objs)
    choice_rule = [
        "1 { visual_mapping(M) : in_visual_mapping_range(M) } 1.", 
        "",
        "in_visual_mapping_range(1.." ++ show (length ms) ++ ")."
        ]
    f (i, m) = map (g i) (zip objs m)
    zms = zip [1..] ms
    g i (obj, p) = "is_visual_type(" ++ obj ++ ", " ++ "vt_type_" ++ show p ++ ") :- visual_mapping(" ++ show i ++ ")."

-- Given a set of objects and a number of visual predicates,
-- generate all the ASP code for low-level visual interpretation
-- that depends on the number of visual predicates
gen_visual_code :: Template -> [String]
gen_visual_code t = h ++ gen_visual_mappings objs num_preds ++ sprite_type_text num_preds ++ looks_constraints_text num_preds ++ bnn_text num_preds ++ [""] ++ visual_sokoban_clauses t where
    h = [divider, "% Low-level visual processing", divider, ""]
    Just num_preds = num_visual_predicates t
    objs = Maybe.mapMaybe f (objects (frame t))
    f (O obj, _) | obj !! 0 /= 'c' = Just (show (O obj))
    f _ = Nothing

-- We assume all types other than "cell" are types of object.
-- This code is specific to Sokoban.
-- We assume all spatial predicates are c_in_<i>.
num_object_types :: Template -> Int
num_object_types t = length (types (frame t)) - 1

-- This code is specific to Sokoban.
visual_sokoban_clauses :: Template -> [String]
visual_sokoban_clauses t = senses_clauses n ++ [""] ++ some_type_at_clauses n ++ [""] ++ something_at_clauses n ++ [""] ++ possible_sprite_at_clauses n ++ [""] where n = num_object_types t

senses_clauses :: Int -> [String]
senses_clauses n = concat (map f [1 .. n]) where
    f i = [
            "1 { senses(s2(c_in_" ++ show i ++ ", Obj, Cell), T) : contains_visual_type(Cell, VT, T) } 1 :-",
            "\tis_visual_type(Obj, VT),",
            "\tVT = vt_type_" ++ show i ++ ",",
            "\tis_time(T),",
            "\tnot is_test_time(T).",
            ""
            ]

some_type_at_clauses :: Int -> [String]
some_type_at_clauses n = concat (map f [1 .. n]) where
    f i = [
            "some_type_at(Cell, VT, T) :- ",
            "\tsenses(s2(c_in_" ++ show i ++ ", X, Cell), T),",
            "\tis_visual_type(X, VT).",
            ""
            ]

something_at_clauses :: Int -> [String]
something_at_clauses n = concat (map f [1 .. n]) where
    f i = [
            "something_at(T, C) :- ",
            "\tis_test_time(T),",
            "\tholds(s2(c_in_" ++ show i ++ ", Obj, C), T).",
            ""
            ]

possible_sprite_at_clauses :: Int -> [String]
possible_sprite_at_clauses n = concat (map f [1 .. n]) where
    f i = [
            "possible_sprite_at(T, C, S) :- ",
            "\tis_test_time(T), ",
            "\tholds(s2(c_in_" ++ show i ++ ", Obj, C), T), ",
            "\tis_visual_type(Obj, VT), ",
            "\tsprite_type(S, VT).    ",
            ""
            ]

sprite_type_text :: Int -> [String]
sprite_type_text num_preds = "" : concat (map f2 its) where
    ts = map f [0 .. num_preds]
    f p = "type_" ++ show p
    its = zip [1..] ts
    f2 (i, t) = ["sprite_type(E, vt_" ++ t ++ ") :-"] ++ g i
    g i = map (h i) [1 .. length ts] ++ [""]
    h i j = "\tbnn_result(E, " ++ show j ++ ", " ++ (if i == j then "1" else "0") ++ ")" ++ (if j == length ts then "." else ",")

looks_constraints_text :: Int -> [String]
looks_constraints_text num_preds = concat (map g ts) where
    ts = map f [0 .. num_preds]
    f p = "type_" ++ show p
    g t = [":- not some_looks_type(vt_" ++ t ++ ").", 
        "some_looks_type(vt_" ++ t ++ ") :- contains_visual_type(C, vt_" ++ t ++ ", T)."]

bnn_text :: Int -> [String]
bnn_text num_preds = ["", n1, n2, n3] where
    n1 = "nodes(1, 25). "
    n2 = "nodes(2, 10)."
    n3 = "nodes(3, " ++ show (num_preds + 1) ++ ")."

