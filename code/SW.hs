module Main where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.Random as Random
import System.Process as Process

import System.Environment

import RandomPairs

type Seq = (String, (String, [Int], TaskType))

all_sequences :: IO [Seq]
all_sequences = do
    let ps = make_prediction_sequence sequences
    let rs = make_retrodiction_sequence sequences
    is <- make_imputation_sequence sequences
    return (ps ++ rs ++ is)

make_prediction_sequence :: [String] -> [(String, (String, [Int], TaskType))]
make_prediction_sequence xs = map f (zip [1..] xs) where
    f (i, x) = (show i, (x, [length x], TaskPrediction))

make_retrodiction_sequence :: [String] -> [(String, (String, [Int], TaskType))]
make_retrodiction_sequence xs = map f (zip [1..] xs) where
    f (i, x) = (show i, (x, [1], TaskRetrodiction))

make_imputation_sequence :: [String] -> IO [Seq]
make_imputation_sequence xs = do
    g <- Random.newStdGen
    let (_, indices) = random_indices g (map length xs)
    let f (i, (j, x)) =  (show i, (x, [j], TaskImputation))
    return $ map f (zip [1..] (zip indices xs))

sequences :: [String]
sequences = c_sequences

nonstationary_sequences :: [String]
nonstationary_sequences = [
    -- One change sequences:
    "aaaaaabcdef",
    "ffffffedcba",
    "abcdeffffff",
    "fedcbaaaaaa",
    -- Two change sequences:
    "abcdeffffffedcba",
    "fedcbaaaaaabcdef",
    "aaaaaabcdeffffff",
    "ffffffedcbaaaaaa"
    ]    

sw_sequences :: [String]
sw_sequences = [
    "aababcabcda",
    "abcde",
    "babbbbbcbbdbbebbf",
    "abbcccdddde",
    "afefafefafefa",
    "babbbcbdbe",
    "abbccddee",
    "abccddeeefff",
    "fafbfcfdf",
    "afeefaafeefaa",
    "bbbccbbbccbbbcc",
    "baabbbaaaabbbbb",
    "bcacacbdbdbcaca",
    "abbccddeeff",
    "aababcabcdabcde",
    "bacabdabceabcdf",
    "abacbadcbaedcb",    
    "cbabcbabcbabcb",
    "aaabbceff",
    "aabaabcbaabcdcb",
    "aabcabbcabccaaa",
    "ababababa",
    "acbdced",
    "acfbead",
    "aaffeedd",
    "aaabbbcc",
    "aabbfabbeabbd",
    "fadabafadaba",
    "abafaaefa",
    "bafbaebad"
    ]

c_sequences :: [String]
c_sequences = [
    "ababab",
    "acbdce",
    "dfcebd",
    "ddccbb",
    -- "ddcfbha", -- out of range
    "aaabbbc",
    -- "dcegfhji", -- out of range
    -- "adgj", -- out of range
    "cabdbccecd",
    "aaaabbbbc",
    -- "eegceac", -- out of range
    -- "abdeg", -- out of range
    "ddeecdee",
    -- "fgeddac", -- out of range
    "efcfab",
    "efedeecde",
    "eeedffc",
    "bbebadb",
    "edfddcfc",
    "fedfecfe",
    "hgf_dcba"
    ]

main :: IO ()
main = do
    seqs <- all_sequences
    Monad.forM_ seqs output_sequence

output_sequence :: (String, (String, [Int], TaskType)) -> IO ()
output_sequence (i, (s, ht, task)) = output_seq i s ht task

k_folder :: String
k_folder = "c-test/"
-- k_folder = "sw/"

output_seq :: String -> String -> [Int] -> TaskType -> IO ()    
output_seq i s hts task = do
    let f = "data/" ++ k_folder ++ show task ++ "_" ++ i ++ ".lp"
    putStrLn $ "Using sequence: " ++ s
    putStrLn $ "Creating file: " ++ f
    writeFile f "%------------------------------------------------------------------------------\n"
    appendFile f $ "% This file was generated using sequence " ++ s ++ "\n"
    appendFile f "%------------------------------------------------------------------------------\n"
    appendFile f "\n"
    appendFile f "% The given sequence\n"
    Monad.forM_ (zip [1..] s) $ \(t, x) -> case t `elem` hts of
        True -> appendFile f $ "hidden(s2(c_letter, obj_sensor, obj_letter_" ++ (x : "), ") ++ show t ++ ").\n"
        False -> appendFile f $ "senses(s2(c_letter, obj_sensor, obj_letter_" ++ (x : "), ") ++ show t ++ ").\n"
    appendFile f "\n"
    appendFile f "% Elements\n"
    appendFile f "is_object(obj_sensor).\n"
    let ts = map show [1 .. length s]
    appendFile f $ "is_time(" ++ concat (List.intersperse ";" ts) ++ ").\n"
    appendFile f "\n"
    appendFile f "% Every sensor has a unique letter attribute\n"
    appendFile f "\n"
    appendFile f "% ∃! clause for c_letter : at most one\n"
    appendFile f "\n"
    appendFile f ":-\n"
    appendFile f "\tholds(s2(c_letter, X, Y), T), \n"
    appendFile f "\tholds(s2(c_letter, X, Y2), T), \n"
    appendFile f "\tY != Y2.\n"
    appendFile f "\n"
    appendFile f "% ∃! clause for c_letter : at least one\n"
    appendFile f "\n"
    appendFile f ":-\n"
    appendFile f "\tpermanent(isa(t_sensor, X)),\n"
    appendFile f "\tis_time(T),\n"
    appendFile f "\tnot aux_c_letter(X, T).\n"
    appendFile f "\n"
    appendFile f "aux_c_letter(X, T) :-\n"
    appendFile f "\tholds(s2(c_letter, X, _), T).\n"
    appendFile f "\n"
    appendFile f "% Incompossibility for letter\n"
    appendFile f "incompossible(s2(c_letter, X, Y), s2(c_letter, X, Y2)) :-\n"
    appendFile f "\tpermanent(isa(t_sensor, X)),\n"
    appendFile f "\tpermanent(isa(t_letter, Y)),\n"
    appendFile f "\tpermanent(isa(t_letter, Y2)),\n"
    appendFile f "\tY != Y2.\n"

write_run_experiments :: IO ()
write_run_experiments = do
    seqs <- all_sequences
    let f = "run_sw_experiments.sh"
    writeFile f (unlines (gen_run_experiments seqs))
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_run_experiments :: [Seq] -> [String] 
gen_run_experiments seqs = h : sws where
    h = "echo \"Running sw experiments...\""
    sws = concat (map g seqs)
    g (i, (x, _, _)) = ["", "echo \"Solving " ++ x ++ "...\"", "time ./solve sw input_" ++ i ++ ".lp"]

write_single_experiment :: IO ()
write_single_experiment = do
    seqs <- all_sequences
    let f = "single_sw_experiment.sh"
    writeFile f (unlines (gen_single_experiment seqs))
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_single_experiment :: [Seq] -> [String]
gen_single_experiment seqs = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f seqs)
    f (i, (x, _, _)) = ["\t" ++ i ++ " )", "\t\techo \"Solving " ++ x ++ "...\"", "\t\ttime ./solve sw input_" ++ i ++ ".lp", "\t\t;;"]
       

write_seq_mnist_experiment :: IO ()
write_seq_mnist_experiment = do
    let f = "single_seq_mnist.sh"
    writeFile f (unlines gen_seq_mnist_experiment)
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_seq_mnist_experiment :: [String]
gen_seq_mnist_experiment = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f (zip [1..] ps))
    ps = [(i, j) | i <- [1..10], j <- [1..5]]
    f (n, (i, j)) = let f = "seq_" ++ show i ++ "_" ++ show j ++ ".lp" in ["\t" ++ show n ++ " )", "\t\techo \"Solving " ++ f ++ "...\"", "\t\ttime code/solve seq-mnist " ++ f, "\t\t;;"]
                    
latex_sequences :: IO [String]
latex_sequences = do
    seqs <- all_sequences    
    let f (_, (s, _, _)) = "\\sequence{" ++ List.intersperse ',' s ++ "}\\\\"
    return $ map f seqs

write_evaluate_baselines :: IO ()
write_evaluate_baselines = do
    let f = "run_baselines_sw.sh"
    writeFile f (unlines evaluate_baselines)
    let c = "chmod 777 " ++ f
    Process.callCommand c

evaluate_baselines :: [String]
evaluate_baselines = xs ++ ys ++ zs where
    xs = ["echo \"Evaluating baselines for sw...\"", 
            "",
            "rm experiments/baselines/sw_*",
            ""
            ]
    ys = map f [1 .. length sequences]
    zs = ["echo \"Correct constant baseline :\"",
            "grep \"baseline_k_sw_correct\" experiments/baselines/sw_* | wc -l",
            "echo \"Total sw examples\"",
            "find experiments/baselines/sw_* | wc -l",
            "echo \"Correct inertia baseline :\"",
            "grep \"baseline_inertia_sw_correct\" experiments/baselines/sw_* | wc -l",
            "echo \"Total sw examples\"",
            "find experiments/baselines/sw_* | wc -l"
            ]
    f t = "clingo --warn=no-atom-undefined pure/baselines.lp data_sw/input_" ++ show t ++ ".lp > experiments/baselines/sw_" ++ show t ++ ".txt"


