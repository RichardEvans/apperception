module Main where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Universe.Helpers as Universe
import qualified System.Environment as Env
import qualified System.Process as Process

import Interpretation
import ExampleTemplates
import SolveTemplates
import OcclusionData

----------------------------------- main --------------------------------------

main :: IO ()
main = do
    args <- Env.getArgs
    putStrLn $ "Solving " ++ concat (List.intersperse " " args)
    case args of
        ["sw", f] -> solve_sw_iteratively f 1
        ["sw", f, n] -> solve_sw_iteratively f (read n)
        ["eca", f] -> solve_eca_iteratively f
        ["eca_general", f] -> solve_eca_general f
        ["music", f] -> solve_music_iteratively f 
        ["rhythm", f] -> solve_rhythm_iteratively f 
        ["misc", f] -> solve_misc f
        ["occlusion", f] -> solve_occlusion f
        ["binding", f] -> solve_binding f
        _ -> putStrLn $ "Usage: solve sw/eca/music/rhythm/misc <file>"


-------------------------------------------------------------------------------
-- Misc-specific solving
-------------------------------------------------------------------------------

solve_misc :: String -> IO ()
solve_misc f = case lookup f misc_templates of
    Nothing -> error $ "No misc template with this id: " ++ f
    Just (dir, template, input) -> process_misc dir template input

process_misc :: String -> Template -> String -> IO ()
process_misc dir t input_f = do
    (results_f, ls2) <- do_solve dir input_f t
    case ls2 of
        [] -> do
            putStrLn "No solution found."
        _ -> do
            let ans = last_answers ls2
            Monad.forM_ ans (write_latex t)
            let ls3 = map (process_answer_with_template t) ans
            Monad.forM_ ls3 putStrLn

-------------------------------------------------------------------------------
-- occlusion-specific iteration
-------------------------------------------------------------------------------

solve_occlusion :: String -> IO ()
solve_occlusion f = case lookup f occlusion_table of
    Nothing -> error $ "No occlusion entry with this id: " ++ f
    Just (dir, template, input) -> process_misc dir template input

occlusion_table :: [(String, (String, Template, String))]
occlusion_table = map f occlusion_worlds where
    f (s, w) = let fn = "input_occlusion_" ++ s ++ ".lp" in 
                let num_cells = max_x w * max_y w in
                let num_objs = length (occ_objects w) in
                (s, ("data/occlusion", template_occlusion fn num_cells num_objs, fn))

-------------------------------------------------------------------------------
-- binding-specific solving
-------------------------------------------------------------------------------

solve_binding :: String -> IO ()
solve_binding f = process_misc "data/binding" template_binding f

-------------------------------------------------------------------------------
-- SW-specific iteration
-------------------------------------------------------------------------------

solve_sw_iteratively :: String -> Int -> IO ()
solve_sw_iteratively input_f num_objects = do
    solve_iteratively "data/sw" input_f (all_sw_templates input_f num_objects) True False

all_sw_templates :: String -> Int -> [(String, Template)]
all_sw_templates input_f n = s ++ c where
    s = map (make_simple_sw_template input_f) [n..3]
    c = map (make_complex_sw_template input_f) [n..3]

make_simple_sw_template :: String -> Int -> (String, Template)
make_simple_sw_template input_f n = update_sw_template_objects t n "simple" where
    t = template_sw_simple n

make_complex_sw_template :: String -> Int -> (String, Template)
make_complex_sw_template input_f n = update_sw_template_objects t n "complex" where
    t = template_sw_complex n

update_sw_template_objects :: Template -> Int -> String -> (String, Template)
update_sw_template_objects t n c = (s, t') where
    f = (frame t) { objects = get_objects t ++ [(O ("gen_" ++ show i), T "cell") | i <- [1..n]]
        }
    t' = t { frame = f } 
    s = "Num objects: " ++ show n ++ " complexity: " ++ c

-------------------------------------------------------------------------------
-- Music-specific iteration
-------------------------------------------------------------------------------
solve_music_iteratively :: String -> IO ()
solve_music_iteratively input_f = do
    solve_iteratively "data/music" input_f (all_music_templates input_f) False  False

all_music_templates :: String -> [(String, Template)]
all_music_templates input_f = map (make_music_template input_f) music_configs

data MusicConfig = MusicConfig { 
    num_extra_fluent_predicates :: Int,
    num_extra_arrow_rules :: Int,
    num_extra_causes_rules :: Int,
    num_extra_body_atoms :: Int
} deriving (Eq, Ord, Show)

music_configs :: [MusicConfig]
music_configs = [
    MusicConfig {
        num_extra_fluent_predicates = 0, 
        num_extra_arrow_rules = 0,
        num_extra_causes_rules = 0,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 0, 
        num_extra_arrow_rules = 0,
        num_extra_causes_rules = 4,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 0, 
        num_extra_arrow_rules = 4,
        num_extra_causes_rules = 4,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 0, 
        num_extra_arrow_rules = 4,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 4, 
        num_extra_arrow_rules = 4,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 4, 
        num_extra_arrow_rules = 8,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 8,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 8,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 1
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 12,
        num_extra_causes_rules = 12,
        num_extra_body_atoms = 1
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 12,
        num_extra_causes_rules = 16,
        num_extra_body_atoms = 1
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 12,
        num_extra_causes_rules = 16,
        num_extra_body_atoms = 2
        }
        ]

make_music_template :: String -> MusicConfig -> (String, Template)
make_music_template input_f c = (show c, template_music') where
    f' = frame_music { fluid_concepts = fluid_concepts f ++ ps }
    f = frame template_music
    ps = [(C ("extra_p" ++ show i), [T "finger"]) | i <- [1..nps]]
    nps = num_extra_fluent_predicates c
    template_music' = template_music { 
        frame = f',
        num_arrow_rules = nar,
        num_causes_rules = ncr,
        max_body_atoms = mba
        }
    nar = num_arrow_rules template_music + num_extra_arrow_rules c
    ncr = num_causes_rules template_music + num_extra_causes_rules c
    mba = max_body_atoms template_music + num_extra_body_atoms c

-------------------------------------------------------------------------------
-- Rhythm-specific iteration
-------------------------------------------------------------------------------
solve_rhythm_iteratively :: String -> IO ()
solve_rhythm_iteratively input_f = do
    solve_iteratively "data/rhythm" input_f (all_rhythm_templates input_f) False False

all_rhythm_templates :: String -> [(String, Template)]
all_rhythm_templates input_f = map (make_rhythm_template input_f) rhythm_configs

rhythm_configs :: [MusicConfig]
rhythm_configs = [
    MusicConfig {
        num_extra_fluent_predicates = 0, 
        num_extra_arrow_rules = 0,
        num_extra_causes_rules = 0,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 0, 
        num_extra_arrow_rules = 0,
        num_extra_causes_rules = 4,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 0, 
        num_extra_arrow_rules = 4,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 4, 
        num_extra_arrow_rules = 4,
        num_extra_causes_rules = 4,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 4, 
        num_extra_arrow_rules = 4,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 4,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 8,
        num_extra_causes_rules = 8,
        num_extra_body_atoms = 1
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 12,
        num_extra_causes_rules = 12,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 12,
        num_extra_causes_rules = 16,
        num_extra_body_atoms = 0
        },
    MusicConfig {
        num_extra_fluent_predicates = 8, 
        num_extra_arrow_rules = 12,
        num_extra_causes_rules = 16,
        num_extra_body_atoms = 1
        }
        ]

make_rhythm_template :: String -> MusicConfig -> (String, Template)
make_rhythm_template input_f c = (show c, template_rhythm') where
    f' = frame_rhythm { fluid_concepts = fluid_concepts f ++ ps }
    f = frame template_rhythm
    ps = [(C ("extra_p" ++ show i), [T "sensor"]) | i <- [1..nps]]
    nps = num_extra_fluent_predicates c
    template_rhythm' = template_rhythm { 
        frame = f',
        num_arrow_rules = nar,
        num_causes_rules = ncr,
        max_body_atoms = mba
        }
    nar = num_arrow_rules template_rhythm + num_extra_arrow_rules c
    ncr = num_causes_rules template_rhythm + num_extra_causes_rules c
    mba = max_body_atoms template_rhythm + num_extra_body_atoms c


-------------------------------------------------------------------------------
-- ECA-specific iteration
-------------------------------------------------------------------------------

solve_eca_iteratively :: String -> IO ()
solve_eca_iteratively input_f = solve_iteratively "data/eca" input_f (all_eca_templates input_f) False False
    
all_eca_templates :: String -> [(String, Template)]
all_eca_templates input_f = map (make_eca_template False input_f) [4..8]

-------------------------------------------------------------------------------
-- ECA iteration using the general code for template iteration
-------------------------------------------------------------------------------

solve_eca_general :: String -> IO ()
solve_eca_general input_f = do
    solve_iteratively "data/misc" input_f (all_general_eca_templates input_f) True True

all_general_eca_templates :: String -> [(String, Template)]
all_general_eca_templates input_f = map f (zip [1..] ts) where
    f (i, t) = ("Template " ++ show i, t)
    ps = parameter_lists [T "sensor"] 100
    ts = map (augment_template t') ps
    t' = template_eca_small

output_general_eca_templates :: String -> Int -> IO ()    
output_general_eca_templates input_f n = Monad.forM_ xs f where
    xs = map snd $ take n (all_general_eca_templates input_f)
    f t = Monad.forM_ (latex_frame t) putStrLn

-------------------------------------------------------------------------------
-- General code for template iteration
-- 
-- This code iterates through templates, generating ever more complex
-- ones, with a guarantee that every possible combination is 
-- eventually reached.
--
-- It relies on Data.Universe.Helpers.choices
-------------------------------------------------------------------------------

type ConceptSpec = (Concept, [Type])

type ObjectSpec = (Object, Type)

type VarSpec = (Var, Type)

data TemplateParameter =   
    IP_ObjectSpecs [ObjectSpec] |
    IP_PermConcept [ConceptSpec] | 
    IP_FluentConcept [ConceptSpec] | 
    IP_VarSpecs [VarSpec] |
    IP_NumArrowRules Int |
    IP_NumCausesRules Int | 
    IP_NumBodyAtoms Int
    deriving (Eq, Ord, Show)    

data TemplateDelta = TD {
    extra_types :: [Type],
    extra_objects :: [ObjectSpec],
    extra_perm_concepts :: [ConceptSpec],
    extra_fluent_concepts :: [ConceptSpec],
    extra_vars :: [VarSpec],
    extra_num_arrow_rules :: Int,
    extra_num_causes_rules :: Int,
    extra_num_body_atoms :: Int
    } deriving (Eq, Ord, Show)  

type IntTypesPair = (Int, [Type]) 

get_first_n_int_pairs :: Int -> [(Int, Int)]
get_first_n_int_pairs n = clip_at_n n int_pairs_for_types

clip_at_n :: Int -> [(Int, Int)] -> [(Int, Int)] 
clip_at_n n ps = res where
    tots = scanl (+) 0 (map fst ps)
    f tot = tot >= n
    Just i = List.findIndex f tots
    ps' = take (i-1) ps
    x = tots !! (i-1)
    diff = n - x
    p = (diff, snd (ps !! (i-1)))
    res = case diff of
        0 -> ps'
        _ -> ps' ++ [p]

get_first_n_int_types_pairs :: Int -> [IntTypesPair]
get_first_n_int_types_pairs n = map f (get_first_n_int_pairs n) where
    f (n, i) = (n, map g [1..i])
    g i = T ("gen_" ++ show i)

const_num_templates_per_type :: Int
const_num_templates_per_type = 100

int_pairs_for_types :: [(Int, Int)]
int_pairs_for_types = ps where
    xs = [const_num_templates_per_type * i | i <- [1..]]
    ys = [0..]
    ps = xs Universe.+*+ ys

parameter_lists :: [Type] -> Int -> [TemplateDelta]
parameter_lists ts n = res where
    res = concat (map f itps)
    itps = get_first_n_int_types_pairs n
    f (i, new_ts) = parameter_lists2 (ts ++ new_ts) new_ts i

parameter_lists2 :: [Type] -> [Type] -> Int -> [TemplateDelta]
parameter_lists2 all_ts new_ts n = take n (all_parameter_lists all_ts new_ts 0)

all_parameter_lists :: [Type] -> [Type] -> Int -> [TemplateDelta]    
all_parameter_lists ts new_ts init_num_objects = res where
    res = map (convert_to_td new_ts) xs
    xs = Universe.choices [num_body_atoms, fluents, object_specs, perms, num_arrows, num_causes, vars]
    perms = map IP_PermConcept (all_concepts ts P)
    fluents = map IP_FluentConcept (all_concepts ts C)
    object_specs = map IP_ObjectSpecs (all_object_specs ts)
    num_arrows = map IP_NumArrowRules[0..]
    num_causes = map IP_NumCausesRules [0..]
    num_body_atoms = map IP_NumBodyAtoms [0..]
    vars = map IP_VarSpecs (all_var_specs ts)

convert_to_td :: [Type] -> [TemplateParameter] -> TemplateDelta
convert_to_td new_ts [IP_NumBodyAtoms n_b, IP_FluentConcept cs, IP_ObjectSpecs objs, IP_PermConcept ps, IP_NumArrowRules n_arrow, IP_NumCausesRules n_cause, IP_VarSpecs vars] = TD {
        extra_types = new_ts,
        extra_objects = objs,
        extra_perm_concepts = ps,
        extra_fluent_concepts = cs,
        extra_vars = vars,
        extra_num_arrow_rules = n_arrow,
        extra_num_causes_rules = n_cause,
        extra_num_body_atoms = n_b
    }

augment_template :: Template -> TemplateDelta -> Template
augment_template template td = template' where
    template' = template {
        frame = frame',
        num_arrow_rules = num_arrow_rules template + extra_num_arrow_rules td,
        num_causes_rules = num_causes_rules template + extra_num_causes_rules td,
        max_body_atoms = max_body_atoms template + extra_num_body_atoms td
    }
    frame' = frm {
        types = types frm ++ extra_types td,
        objects = get_objects template ++ extra_objects td,
        permanent_concepts = permanent_concepts frm ++ map f (extra_perm_concepts td),
        fluid_concepts = fluid_concepts frm ++ extra_fluent_concepts td,
        vars = vars frm ++ extra_vars td,
        var_groups = vgs
    }
    vgs = case (extra_vars td) of
        [] -> var_groups frm
        vs -> var_groups frm ++ [last (var_groups frm) ++ map fst vs]
    frm = frame template
    f (c, ts) = (c, Constructed, ts)

show_parameters :: TemplateDelta -> String     
show_parameters td = unlines xs where
    xs = types_t ++ objs_t ++ perms_t ++ fluents_t ++ vars_t ++ [arrows_t, causes_t, body_atoms_t]
    arrows_t = "Num extra arrow rules: " ++ show (extra_num_arrow_rules td)
    causes_t = "Num extra causes rules: " ++ show (extra_num_causes_rules td)
    body_atoms_t = "Num extra body atoms: " ++ show (extra_num_body_atoms td)
    types_t = case extra_types td of
        [] -> ["No extra types"]
        ts -> "Extra types: " : map show ts
    objs_t = case extra_objects td of
        [] -> ["No extra objects"]
        objs -> "Extra objects: " : map show objs
    perms_t = case extra_perm_concepts td of 
        [] -> ["No extra permanent concepts"]
        ps -> "Extra permanent concepts: " : map show ps
    fluents_t = case extra_fluent_concepts td of 
        [] -> ["No extra fluent concepts"]
        fs -> "Extra fluent concepts: " : map show fs
    vars_t = case extra_vars td of
        [] -> ["No extra vars"]
        vars -> "Extra vars: " : map show vars

all_concepts :: [Type] -> (String -> Concept) -> [[ConceptSpec]]
all_concepts ts f = concat [ all_concepts_of_length ts f i | i <- [0..]]

all_concepts_of_length :: [Type] -> (String -> Concept) -> Int -> [[ConceptSpec]]
all_concepts_of_length _ _ 0 = [[]]
all_concepts_of_length ts f n = r where
    r = [x ++ [c] | x <- all_concepts_of_length ts f (n-1), c <-all_concepts_with_index ts f n]

all_concepts_with_index :: [Type] -> (String -> Concept) -> Int -> [ConceptSpec]
all_concepts_with_index ts f i = unaries ++ binaries where
    unaries = [(f ("gen_" ++ show i), [t]) | t <- ts]
    binaries = [(f ("gen_" ++ show i), [t, t2]) | t <- ts, t2 <- ts]

all_object_specs :: [Type] -> [[ObjectSpec]]
all_object_specs ts = concat [all_object_specs_of_length ts i | i <- [0..]]

all_object_specs_of_length :: [Type] -> Int -> [[ObjectSpec]]
all_object_specs_of_length _ 0 = [[]]
all_object_specs_of_length ts n = r where
    r = [x ++ [c] | x <- all_object_specs_of_length ts (n-1), c <-all_object_specs_with_index ts n]

all_object_specs_with_index :: [Type] -> Int -> [ObjectSpec]
all_object_specs_with_index ts i = [(O ("gen_" ++ show i), t) | t <- ts]

all_var_specs :: [Type] -> [[VarSpec]]
all_var_specs ts = concat [all_var_specs_of_length ts i | i <- [0..]]

all_var_specs_of_length :: [Type] -> Int -> [[VarSpec]]
all_var_specs_of_length _ 0 = [[]]
all_var_specs_of_length ts n = r where
    r = [x ++ [c] | x <- all_var_specs_of_length ts (n-1), c <-all_var_specs_with_index ts n]

all_var_specs_with_index :: [Type] -> Int -> [VarSpec]
all_var_specs_with_index ts i = [(V ("gen_" ++ show i), t) | t <- ts]

-------------------------------------------------------------------------------
-- Solving iteratively
-------------------------------------------------------------------------------

solve_iteratively dir input_f ts continue output_intermediaries = solve_iteratively2 dir input_f ts continue output_intermediaries Nothing where
    max_int = maxBound :: Int
    max_int_s = show max_int

solve_iteratively2 :: String -> String -> [(String, Template)] -> Bool -> Bool -> Maybe ClingoResult -> IO ()
solve_iteratively2 dir input_f [] False _ _ = putStrLn $ "Unable to solve " ++ input_f
solve_iteratively2 dir input_f [] True _ Nothing = putStrLn $ "Unable to solve " ++ input_f
solve_iteratively2 dir input_f [] True _ (Just r) = do
    let t = result_template r
    putStrLn $ process_answer_with_template t (Answer (result_answer r))
    putStrLn $ process_answer_with_template t (Optimization (result_optimization r))    
solve_iteratively2 dir input_f ((s, t) : ts) continue output_intermediary_results r = do
        putStrLn s
        (results_f, ls2) <- do_solve dir input_f t
        case ls2 of
            [] -> do
                putStrLn "No solution found for this configuration"
                putStrLn ""
                solve_iteratively2 dir input_f ts continue output_intermediary_results r
            _ -> do
                let last_answer = last_answers ls2
                case output_intermediary_results || (not continue) of
                    True -> do
                        let ls3 = map (process_answer_with_template t) last_answer
                        Monad.forM_ ls3 putStrLn
                    False -> return ()
                case continue of
                    False -> return ()
                    True -> do
                        let r' = update_best t r last_answer
                        solve_iteratively2 dir input_f ts continue output_intermediary_results r'

update_best :: Template -> Maybe ClingoResult -> [ClingoOutput] -> Maybe ClingoResult
update_best t Nothing [Answer a, Optimization s] = Just r where
    r = CR { result_answer = a, result_optimization = s, result_template = t }
update_best t (Just r) [Answer a, Optimization s] = let (old_int, new_int) = (read (result_optimization r), read s) :: (Int, Int) in case new_int < old_int of
    True -> Just (CR { result_answer = a, result_optimization = s, result_template = t })
    False -> Just r

do_solve :: String -> String -> Template -> IO (String, [ClingoOutput])
do_solve dir input_f t = do
    -- Generate ASP files from template
    putStrLn "Generating temporary files..."
    (name, command, results) <- do_template False t dir input_f 

    -- Call ASP solver
    putStrLn "Calling clingo..."    
    Exception.catch (Process.callCommand command) handle_command_exception

    -- Process output
    l <- readFile results
    let ls = lines l
    let ls2 = get_answers ls []    

    -- Clean up
    case flag_delete_temp of
        True -> do
            let c = "rm temp/" ++ name ++ "_*"
            Process.callCommand c
        False -> return ()

    return (results, ls2)

-- Calling the system command raises an exception...
-- but the command executes properly...
-- ...so I just ignore the exception...
handle_command_exception :: Exception.SomeException -> IO ()
handle_command_exception _ = return ()

-------------------------------------------------------------------------------
-- Generate increasingly complex templates...
--
-- This code is used to generate .lp files based on templates
-- of increasing complexity, so we can examine how quickly the number of 
-- ground atoms grows as a function of the template parameters.
-------------------------------------------------------------------------------

generate_eca_with_increasing_vars :: IO ()
generate_eca_with_increasing_vars = Monad.forM_ [1 .. 6] generate_eca_with_extra_vars

generate_eca_with_extra_vars :: Int -> IO ()
generate_eca_with_extra_vars n = do
    let vars' =  [(V ("s" ++ show i), T "sensor") | i <- [1..n]]
    let vgs = [[V ("s" ++ show i) | i <- [1..n]]]
    let t = template_eca_n False 11
    let frame' = (frame t) { vars = vars', var_groups = vgs} 
    let t'= t { frame = frame'}
    _ <- do_template False t' "TODO" "TODO"
    return ()

generate_music_with_increasing_vars :: IO ()
generate_music_with_increasing_vars = Monad.forM_ [1 .. 10] generate_music_with_extra_vars

extra_music_vars :: [(Int, String)]
extra_music_vars = [(i, j) | i <- [1..5], j <- ["finger", "loudness", "sensor"]]

get_n_music_vars :: Int -> [(Var, Type)]
get_n_music_vars n = map f [1..n] where
    f x = let (i, j) = extra_music_vars !! (x-1) in (V (head j : show i), T j)

generate_music_with_extra_vars :: Int -> IO ()
generate_music_with_extra_vars n = do
    let vars' = get_n_music_vars n
    let vgs = [map fst vars']
    let t = template_music
    let frame' = (frame t) { vars = vars', var_groups = vgs} 
    let t'= t { frame = frame'}
    _ <- do_template False t' "TODO" "TODO"
    return ()

