module Main where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Text.Printf as Printf
import System.Process as Process

import Data.Char (intToDigit)
import Numeric (showHex, showIntAtBase)
import System.Environment

import RandomPairs

input_prefix :: String
input_prefix = "new_eca_input_r"

-------------------------------------- Types ----------------------------------

data Bin = On | Off deriving (Eq, Ord)

type Context = (Bin, Bin, Bin)

data Binary = B [Bin] deriving (Eq, Ord)

type Rule = Map.Map Context Bin

-------------------------------------- Show -----------------------------------

instance Show Bin where
    show On =   "⬛"
    show Off =  "⬜"

instance Show Binary where
    show (B bs) = concat (map show bs)    

-------------------------------------- Step -----------------------------------

step :: Rule -> Binary -> Binary
step r b = B (map (apply_rule r) cs) where
    cs = map (get_context b) [0 .. blength b - 1]

blength :: Binary -> Int
blength (B bs) = length bs

apply_rule :: Rule -> Context -> Bin
apply_rule r c = case Map.lookup c r of
    Just b -> b
    Nothing -> error $ "Map lookup failed for context " ++ show c

get_context :: Binary -> Int -> Context
get_context (B bs) 0 = (x, y, z) where
    x = bs !! (length bs - 1)
    y = bs !! 0
    z = bs !! 1
get_context (B bs) i | i == length bs - 1 = (x, y, z) where
    x = bs !! (i - 1)
    y = bs !! i
    z = bs !! 0
get_context (B bs) i | otherwise = (x, y, z) where
    x = bs !! (i - 1)
    y = bs !! i
    z = bs !! (i + 1)

process_rule :: Int -> Binary -> Int -> [Binary]
process_rule i b n = process (int_to_rule i) b n

process :: Rule -> Binary -> Int -> [Binary]
process r b n = b : process2 r b (n - 1)

process2 :: Rule -> Binary -> Int -> [Binary]
process2 _ _ 0 = []
process2 r b n = b' : process2 r b' (n-1) where
    b' = step r b

int_to_rule :: Int -> Rule
int_to_rule n = Map.fromList ps where
    ps = zip all_contexts bs
    bs = convert_to_bin n

all_contexts :: [Context]
all_contexts = [
    (On, On, On),
    (On, On, Off),
    (On, Off, On),
    (On, Off, Off),
    (Off, On, On),
    (Off, On, Off),
    (Off, Off, On),
    (Off, Off, Off)
    ]

convert_to_bin :: Int -> [Bin]
convert_to_bin n = map f s2 where
    s = showIntAtBase 2 intToDigit n ""
    s2 = replicate k '0' ++ s
    k = 8 - length s
    f '0' = Off
    f '1' = On

print_rule :: Int -> Binary -> Int -> IO ()
print_rule ri b n = do
    let bs = process_rule ri b n
    Monad.forM_ bs print

k_max_touch :: Int
k_max_touch = 3

touch_sensor_readings :: [Int] -> [Binary] -> [[Int]]    
touch_sensor_readings tss bs = decay_touch_sensor_readings tsrs where
    tsrs = map f bs
    f (B xs) = map (g xs) tss
    g xs ts = case xs !! ts of
        On -> k_max_touch
        Off -> 0

decay_touch_sensor_readings :: [[Int]] -> [[Int]]
decay_touch_sensor_readings tsrs = decay_touch_sensor_readings2 tsrs xs where
    xs = replicate n 0
    n = length (head tsrs)

decay_touch_sensor_readings2 :: [[Int]] -> [Int] -> [[Int]]
decay_touch_sensor_readings2 [] _ = []
decay_touch_sensor_readings2 (x:xs) y = (x':xs') where
    x' = map f (zip x y)
    xs' = decay_touch_sensor_readings2 xs x'
    f (n1, n2) | n2 > n1 = n2 - 1
    f (n1, n2) | otherwise = n1

output_rule :: String -> Example -> IO ()
output_rule dir e = do
    let (ri, bi, t, ot, ns) = (rule_index e, start_index e, num_times e, num_observed_times e, num_sensors e)
    let b = start_config !! bi
    let tt = show (task_type e)
    let f = dir ++ "/" ++ tt ++ "_" ++ show ri ++ "_b" ++ show bi ++ "_t" ++ show t ++ ".lp"
    output_rule2 dir f ri bi t ot ns b (touch_sensors e) (task_type e)

output_rule2 :: String -> String -> Int -> Int -> Int -> Int -> Int -> Binary -> [Int] -> TaskType -> IO ()    
output_rule2 dir f ri bi t ot ns b touch_ss task_type = do
    let bs = process_rule ri b t
    putStrLn $ "Using rule " ++ show ri
    putStrLn $ "Using initial configuration: " ++ show b
    putStrLn ""
    putStrLn $ "Generating " ++ f
    writeFile f "%------------------------------------------------------------------------------\n"
    appendFile f $ "% This file was generated using rule " ++ show ri ++ "\n"
    appendFile f $ "% with configuration " ++ show b ++ "\n"
    appendFile f "%------------------------------------------------------------------------------\n"
    appendFile f "\n"
    appendFile f "%------------------------------------------------------------------------------\n"
    appendFile f "% The sensory given\n"
    appendFile f "%------------------------------------------------------------------------------\n"
    appendFile f "\n"
    appendFile f "%------------------------------------------------------------------------------\n"
    appendFile f "% Time  State\n"
    appendFile f "%\n"
    Monad.forM_ (zip [1..] bs) $ \(i, b) -> appendFile f $ "% " ++ pad_int i ++ "\t" ++ show b ++ "\n"
    appendFile f "%------------------------------------------------------------------------------\n"
    appendFile f "\n"
    appendFile f "% The given sequence\n"
    ps <- hidden_pairs task_type (length bs) ns
    Monad.forM_ (zip [1..] bs) $ \(i, B b) -> do
        Monad.forM_ (zip [1..ns] b) $ \(j, x) -> do
            let p = if x == On then "c_on" else "c_off"
            let prd = if (i,j) `elem` ps then "hidden" else "senses"
            appendFile f $ prd ++ "(s(" ++ p ++ ", obj_cell_" ++ show j ++ "), " ++ show i ++ ").\n"
    appendFile f "\n"
    case touch_ss of
        [] -> return ()
        ts -> do
            let tsrs = touch_sensor_readings touch_ss bs
            appendFile f "% The touch sensors\n"
            Monad.forM_ (zip [1..] tsrs) $ \(i, tsr) -> do
                Monad.forM_ (zip [1..ns] tsr) $ \(j, x) -> do
                    let prd = if (i, (touch_ss !! (j-1)) + 1 ) `elem` ps then "hidden" else "senses"
                    appendFile f $ prd ++ "(s2(c_touch, obj_touch_sensor_" ++ show j ++ ", " ++ "obj_touch_" ++ show x ++ "), " ++ show i ++ ").\n"
            appendFile f "\n"
    appendFile f "% Elements\n"
    let nobjs = min (blength b) ns
    let objs = map (\i -> "obj_cell_" ++ show i) [1 .. nobjs]
    Monad.forM_ objs $ \x -> appendFile f $ "is_object(" ++ x ++ ").\n"
    let touch_sensor_objs = map (\i -> "obj_touch_sensor_" ++ show i) [1 .. length touch_ss]
    case touch_sensor_objs of 
        [] -> return ()
        _ -> appendFile f $ "is_object(" ++ concat (List.intersperse ";" touch_sensor_objs) ++ ").\n"
    let ts = map show [1..t] 
    appendFile f $ "is_time(1.." ++ show (length ts) ++ ").\n"
    appendFile f $ "is_concept(c_on).\n"
    appendFile f $ "is_concept(c_off).\n"
    appendFile f "\n"
    appendFile f "% Input exclusions\n"
    appendFile f "% Every sensor is either on or off\n"
    appendFile f "% S : sensor → on(S) ⊕ off(S)\n"
    appendFile f "\n"
    appendFile f "% At most one\n"
    appendFile f ":-\n"
    appendFile f "\tholds(s(c_on, X), T),\n"
    appendFile f "\tholds(s(c_off, X), T).\n"
    appendFile f "\n"
    appendFile f "% At least one\n"
    appendFile f ":-\n"
    appendFile f "\tpermanent(isa(t_sensor, X)),\n"
    appendFile f "\tis_time(T),\n"
    appendFile f "\tnot holds(s(c_on, X), T),\n"
    appendFile f "\tnot holds(s(c_off, X), T).\n"
    appendFile f "\n"
    appendFile f "% Incompossibility\n"
    appendFile f "incompossible(s(c_on, X), s(c_off, X)) :-\n"
    appendFile f "\tpermanent(isa(t_sensor, X)).\n"
    appendFile f "\n"
    appendFile f "exclusion_output(\"c_on+c_off\").\n"
    case touch_ss of
        [] -> return ()
        _ -> appendFile f (unlines add_exclusions_for_touch_sensors)
     
add_exclusions_for_touch_sensors :: [String]     
add_exclusions_for_touch_sensors = [
    "",
    "% Touch sensor exclusions",
    "",
    "is_concept(c_touch).",
    "",
    "% ∃! clause for c_touch : at most one",
    ":-",
    "\tholds(s2(c_touch, X, Y), T),",
    "\tholds(s2(c_touch, X, Y2), T),",
    "\tY != Y2.",
    "",
    "% ∃! clause for c_touch : at least one",
    ":-",
    "\tpermanent(isa(t_touch_sensor, X)),",
    "\tis_time(T),",
    "\tnot aux_c_touch(X, T).",
    "",
    "aux_c_touch(X, T) :-",
    "\tholds(s2(c_touch, X, _), T).",
    "",
    "% Incompossibility for p_r",
    "incompossible(s2(c_touch, X, Y), s2(c_touch, X, Y2)) :-",
    "\tpermanent(isa(t_touch_sensor, X)),",
    "\tpermanent(isa(t_touch, Y)),",
    "\tpermanent(isa(t_touch, Y2)),",
    "\tY != Y2."
    ]

pad_int :: Int -> String
pad_int i = Printf.printf "%3d" i


-------------------------------------- Main ----------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["all"] -> do
            Monad.forM_ all_examples $ \e -> output_rule "data/eca" e
            write_single_experiment
        ["binding"] -> do_binding_examples
        _ -> do
            putStrLn $ "Usage: eca [all/binding]"

do_binding_examples :: IO ()
do_binding_examples = do
    Monad.forM_ binding_examples $ \e -> output_rule "data/misc" e
    output_binding_single_experiment

data Example = E {
    rule_index :: Int,
    start_index :: Int,
    num_times :: Int,
    num_observed_times :: Int, -- this must be <= num_times
    num_sensors :: Int, -- this must be <= blength initial_state
    touch_sensors :: [Int],
    task_type :: TaskType
}


initial_states :: [Int]
initial_states = [5]

different_times :: [Int]
different_times = [14]

all_examples :: [Example]
all_examples = [
    E { rule_index = i, start_index = j,  num_times = t, 
        num_observed_times = t-1, num_sensors = 11, 
        touch_sensors = [], task_type = task } | 
            i <- [0..255], 
            j <- initial_states, 
            t <- different_times, 
            task <- all_task_types]

all_task_types :: [TaskType]
all_task_types = [TaskPrediction, TaskRetrodiction, TaskImputation]

gen_single_experiment :: [String]
gen_single_experiment = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    ps = [(i, j, t) | i <- [0..255], j <- initial_states, t <- different_times]
    xs = concat (map f (zip [1..] ps))
    f (n, (i, j, t)) = ["\t" ++ show n ++ " )", "\t\techo \"Solving eca r" ++ show i ++ ", b" ++ show j ++ ", t" ++ show t ++ "...\"", "\t\ttime ./solve eca " ++ input_prefix ++ show i ++ "_b" ++ show j ++ "_t" ++ show t ++ ".lp", "\t\t;;"]

write_single_experiment :: IO ()
write_single_experiment = do
    let f = "single_eca_experiment.sh"
    writeFile f (unlines gen_single_experiment)
    let c = "chmod 777 " ++ f
    Process.callCommand c

output_binding_single_experiment :: IO ()    
output_binding_single_experiment = do
    let f = "single_binding_experiment.sh"
    writeFile f (unlines gen_binding_experiment)
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_binding_experiment :: [String]
gen_binding_experiment = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f (zip [1..] binding_examples))
    f (n, e) = let (i, j) = (rule_index e, start_index e) in ["\t" ++ show n ++ " )", "\t\techo \"Solving binding r" ++ show i ++ ", b" ++ show j ++ "...\"", "\t\ttime ./solve binding " ++ input_prefix ++ show i ++ "_b" ++ show j ++ "_t14.lp", "\t\t;;"]

write_evaluate_baselines :: IO ()
write_evaluate_baselines = do
    let f = "run_baselines_eca.sh"
    writeFile f (unlines evaluate_baselines)
    let c = "chmod 777 " ++ f
    Process.callCommand c

evaluate_baselines :: [String]
evaluate_baselines = xs ++ ys ++ zs where
    xs = ["echo \"Evaluating baselines for eca...\"", 
            "",
            "rm experiments/baselines/eca_*",
            ""
            ]
    ys = map f ws
    ws = ["r" ++ show i ++ "_b" ++ show j | i <- [0..255], j <- initial_states]
    zs = ["echo \"Correct constant baseline :\"",
            "grep \"baseline_k_eca_correct\" experiments/baselines/eca_* | wc -l",
            "echo \"Total eca examples\"",
            "find experiments/baselines/eca_* | wc -l",
            "echo \"Correct inertia baseline :\"",
            "grep \"baseline_inertia_eca_correct\" experiments/baselines/eca_* | wc -l",
            "echo \"Total eca examples\"",
            "find experiments/baselines/eca_* | wc -l"
            ]
    f t = "clingo --warn=no-atom-undefined pure/baselines.lp data/eca/input_" ++ t ++ ".lp > experiments/baselines/eca_" ++ t ++ ".txt"

output_eca_ilasp_examples :: IO ()
output_eca_ilasp_examples = do
    let ps = zip [2..11] (map init_b [2..11])
    Monad.forM_ ps $ \(i,b) -> output_rule2 "data/eca" ("eca_ilasp_r245_" ++ show i ++ ".lp") 245 i 6 5 i b [] TaskPrediction

-------------------------------------- Data --------------------------------

start_config :: [Binary]
start_config = [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]

-- These are various small configurations
b0 :: Binary
b0 = B $ replicate 4 Off ++ [On] ++ replicate 4 Off

b1 :: Binary
b1 = B $ replicate 20 Off ++ [On] ++ replicate 20 Off

b2 :: Binary
b2 = B $ replicate 2 Off ++ [On] ++ replicate 2 Off

b3 :: Binary
b3 = B $ [On, On, Off]

b4 :: Binary
b4 = B $ replicate 9 Off ++ [On]

-- b5 to b14 are the ten configurations of length 11
-- These are the configurations used in the experiments.
b5 :: Binary
b5 = B $ replicate 5 Off ++ [On] ++ replicate 5 Off

b6 :: Binary
b6 = B $ [Off, Off, Off, On, Off, Off, Off, On, Off, Off, On]

b7 :: Binary
b7 = B $ [Off, On, Off, On, Off, On, Off, On, Off, On, Off]

b8 :: Binary
b8 = B $ [Off, On, On, On, Off, On, Off, On, On, On, Off]

b9 :: Binary
b9 = B $ [Off, Off, Off, On, Off, Off, Off, On, On, Off, Off]

b10 :: Binary
b10 = B $ [Off, On, On, On, Off, On, Off, On, On, Off, Off]

b11 :: Binary
b11 = B $ [On, Off, On, Off, On, Off, On, Off, On, Off, On]

b12 :: Binary
b12 = B $ replicate 5 On ++ [Off] ++ replicate 5 On

b13 :: Binary
b13 = B $ [Off, On, Off, On, Off, Off, Off, Off, Off, Off, On]

b14 :: Binary
b14 = B $ [On, Off, Off, On, Off, Off, On, On, Off, Off, Off]

init_b :: Int -> Binary
init_b 0 = B []
init_b n | n `rem` 2 == 0 = let B x = init_b (n-1) in
    B (Off : x)
init_b n | n `rem` 2 == 1 = let B x = init_b (n-1) in
    B (On : x)

binding_examples :: [Example]
binding_examples = [binding_example1, binding_example2, binding_example3, binding_example4, binding_example5, binding_example6, binding_example7, binding_example8, binding_example9, binding_example10, binding_example11, binding_example12, binding_example13, binding_example14, binding_example15, binding_example16, binding_example17, binding_example18, binding_example19, binding_example20]
    
binding_example1 :: Example
binding_example1 = E {
    rule_index = 110,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [2, 10],
    task_type = TaskPrediction
}

binding_example2 :: Example
binding_example2 = E {
    rule_index = 2,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [2, 8],
    task_type = TaskPrediction
}

binding_example3 :: Example
binding_example3 = E {
    rule_index = 11,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [1, 5],
    task_type = TaskPrediction
}

binding_example4 :: Example
binding_example4 = E {
    rule_index = 13,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 7],
    task_type = TaskPrediction
}

binding_example5 :: Example
binding_example5 = E {
    rule_index = 25,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 5],
    task_type = TaskPrediction
}

binding_example6 :: Example
binding_example6 = E {
    rule_index = 26,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example7 :: Example
binding_example7 = E {
    rule_index = 30,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 5],
    task_type = TaskPrediction
}

binding_example8 :: Example
binding_example8 = E {
    rule_index = 61,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example9 :: Example
binding_example9 = E {
    rule_index = 67,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 5],
    task_type = TaskPrediction
}

binding_example10 :: Example
binding_example10 = E {
    rule_index = 90,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example11 :: Example
binding_example11 = E {
    rule_index = 133,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 1],
    task_type = TaskPrediction
}

binding_example12 :: Example
binding_example12 = E {
    rule_index = 135,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example13 :: Example
binding_example13 = E {
    rule_index = 139,
    start_index = 5,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example14 :: Example
binding_example14 = E {
    rule_index = 147,
    start_index = 6,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example15 :: Example
binding_example15 = E {
    rule_index = 148,
    start_index = 13,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example16 :: Example
binding_example16 = E {
    rule_index = 155,
    start_index = 8,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example17 :: Example
binding_example17 = E {
    rule_index = 158,
    start_index = 14,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example18 :: Example
binding_example18 = E {
    rule_index = 167,
    start_index = 11,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example19 :: Example
binding_example19 = E {
    rule_index = 176,
    start_index = 13,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 4],
    task_type = TaskPrediction
}

binding_example20 :: Example
binding_example20 = E {
    rule_index = 193,
    start_index = 9,
    num_times = 14,
    num_observed_times = 13,
    num_sensors = 11,
    touch_sensors = [0, 1],
    task_type = TaskPrediction
}

