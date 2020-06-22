module Main where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Environment as Environment
import qualified System.Random as Random
import qualified System.Process as Process

k_sequence_lengths :: [Int]
-- k_sequence_lengths = [5, 10, 20, 50]
k_sequence_lengths = [100]

k_max_sequence_length :: Int
k_max_sequence_length = last k_sequence_lengths

k_max_mislabellings :: Int
k_max_mislabellings = 50

k_mislabelling_step :: Int
k_mislabelling_step = 5

k_num_variants :: Int
k_num_variants = 5

data SeqData = SeqData {
    seq_id :: Int,
    seq_length :: Int,
    seq_num_mutations :: Int,
    seq_index :: Int,
    seq_seq :: [Int]
    } deriving (Eq, Ord, Show)

print_mutated_sequences :: IO ()
print_mutated_sequences = do
    ss <- create_mutated_sequences
    Monad.forM_ ss print

create_mutated_sequences :: IO [SeqData]
create_mutated_sequences = do
    xs <- Monad.mapM mutate_sequence (zip [1..] sequences)
    return (concat xs)

mutate_sequence :: (Int, [Int]) -> IO [SeqData]
mutate_sequence (i, s) = do
    let ms = map (* k_mislabelling_step) [0 .. k_max_mislabellings `div` k_mislabelling_step]
    let ps = [(len, mis) | len <- k_sequence_lengths, mis <- ms]
    xs <- Monad.mapM (mutate_n s i) ps
    return (concat xs)

mutate_n :: [Int] -> Int -> (Int, Int) -> IO [SeqData]
mutate_n s i (len, j) | j == 0 = Monad.mapM (mutate_2 s i len j) [1]
mutate_n s i (len, j) | otherwise = Monad.mapM (mutate_2 s i len j) [1 .. k_num_variants]

mutate_2 :: [Int] -> Int -> Int -> Int -> Int -> IO SeqData
mutate_2 s i len j k = do
    r <- Random.newStdGen
    let s' = mutate r (take len s) j
    return SeqData {
        seq_id = i,
        seq_length = len,
        seq_num_mutations = j,
        seq_index = k,
        seq_seq = s'
        }    

sequences :: [[Int]]
sequences = [
    take k_max_sequence_length x1,
    take k_max_sequence_length x2,
    take k_max_sequence_length x3,
    take k_max_sequence_length x4,
    take k_max_sequence_length x5,
    take k_max_sequence_length x6,
    take k_max_sequence_length x7,
    take k_max_sequence_length x8,
    take k_max_sequence_length x9,
    take k_max_sequence_length x10
    ] where 
        x1 = [1,2] ++ x1
        x2 = [1,1,2] ++ x2
        x3 = [1,1,2,2] ++ x3 
        x4 = [1,1,1,2] ++ x4
        x5 = [1,2,2,1] ++ x5
        x6 = [1,2,3] ++ x6
        x7 = [1,2,3,2,1] ++ x7
        x8 = [1,2,1,3] ++ x8
        x9 = [1,2,3,3] ++ x9
        x10 = [1,1,2,2,3,3] ++ x10

mutate :: Random.RandomGen g => g -> [Int] -> Int -> [Int]
mutate g s n = s2 where
    (_, s2) = List.foldl' f (g2, s) indices 
    (g2, indices) = pick_index_list g s n
    f (g', s') i = let (g'', c) = pick g' (List.delete (s !! i) ls) in (g'', seq_replace s' i c)
    ls = List.nub s

seq_replace :: [Int] -> Int -> Int -> [Int]
seq_replace s i c = take i s ++ [c] ++ drop (i+1) s

pick_index_list :: Random.RandomGen g => g -> [Int] -> Int -> (g, [Int])
pick_index_list g s i = (g3, ss) where
    l = length s - 2 -- We never mutate the last element as it is held-out.
    (g3, ss, _) = List.foldl' f (g, [], [0 .. l]) [1 .. i]
    f (g', s', is) _ = (g2, x:s', List.delete x is) where (g2, x) = pick g' is

pick :: Random.RandomGen r => r -> [a] -> (r, a)
pick _ [] = error "pickFromList wrongly given empty list"
pick r as =
    let indices = (0, length as-1)
        (i, r') = Random.randomR indices r
    in  (r', as !! i)

sequence_to_string :: [Int] -> [String]
sequence_to_string s = result where
    result = header ++ senses ++ elements
    header = [d, h, d]
    d = "%------------------------------------------------------------------------------"
    h = "% This file was generated using sequence " ++ concat (map show s)
    senses = "" : map f (zip [1..] s)
    f (t, x) | t < length s = "senses(s(c_" ++ show x ++ ", obj_sensor), " ++ show t ++ ")."
    f (t, x) | otherwise = "hidden(s(c_" ++ show x ++ ", obj_sensor), " ++ show t ++ ")."
    elements = "" : e : obj : obj_t : ts
    e = "% Elements"
    obj = "is_object(obj_sensor)."
    obj_t = "permanent(isa(t_sensor, obj_sensor))."
    ts = map ft [1 .. length s]
    ft t = "is_time(" ++ show t ++ ")."

main :: IO ()
main = do
    ss <- create_mutated_sequences
    Monad.forM_ ss output_sequence
    write_single_experiment ss

output_sequence :: SeqData -> IO ()   
output_sequence sd = do
    let f = "data/mislabel/" ++ sd_file sd
    let ss = sequence_to_string (seq_seq sd)
    writeFile f (unlines ss)

sd_file :: SeqData -> String    
sd_file sd = "predict_" ++ show (seq_id sd) ++ "_" ++ show (seq_length sd) ++ "_" ++ show (seq_num_mutations sd) ++ "_" ++ show (seq_index sd) ++ ".lp"

write_single_experiment :: [SeqData] -> IO ()
write_single_experiment sds = do
    let f = "scripts/single_mislabel_experiment.sh"
    writeFile f (unlines (gen_single_experiment sds))
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_single_experiment :: [SeqData] -> [String]
gen_single_experiment sds = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f (zip [0..] sds))
    f (i, sd) = ["\t" ++ show i ++ " )", "\t\techo \"Solving " ++ sd_file sd ++ "...\"", "\t\ttime code/solve mislabel " ++ sd_file sd, "\t\t;;"]
       
