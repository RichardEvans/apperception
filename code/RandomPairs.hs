module RandomPairs where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Text.Printf as Printf
import System.Process as Process
import qualified System.Random as Random

import Data.Char (intToDigit)
import Numeric (showHex, showIntAtBase)
import System.Environment


-------------------------------------- Flags ----------------------------------

-- If task type is prediction, then the hidden sensory readings
-- are placed at the end of the sequence.
-- 
-- If task type is retrodiction, then the hidden sensory readings
-- are placed at the beginning of the sequence.
-- 
-- If task type is imputation, then the hidden sensory readings
-- are placed randomly within the sequence.
data TaskType = TaskPrediction | TaskRetrodiction | TaskImputation

instance Show TaskType where
    show TaskPrediction = "predict"
    show TaskRetrodiction = "retrodict"
    show TaskImputation = "impute"

-------------------------------------- Random selection -----------------------

hidden_pairs :: TaskType -> Int -> Int -> IO [(Int, Int)]
hidden_pairs task_type max_i max_j = case task_type of 
    TaskPrediction -> return [(max_i, j) | j <- [1 .. max_j]]
    TaskRetrodiction -> return [(1, j) | j <- [1 .. max_j]]
    TaskImputation -> random_pairs max_i max_j max_j

random_pairs :: Int -> Int -> Int -> IO [(Int, Int)]
random_pairs max_i max_j n = do
    g <- Random.newStdGen
    return (get_n_pairs g max_i max_j n [])

get_n_pairs :: Random.RandomGen g => g -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
get_n_pairs _ _ _ 0 acc = acc
get_n_pairs g max_i max_j n acc = get_n_pairs g' max_i max_j (n-1) (a : acc) where (g', a) = get_pair g max_i max_j acc 

get_pair :: Random.RandomGen g => g -> Int -> Int -> [(Int, Int)] -> (g, (Int, Int))
get_pair g max_i max_j sofar = let (g', a) = pick_pair g max_i max_j in
    case a `elem` sofar of 
        True -> get_pair g' max_i max_j sofar
        False -> (g', a)

pick_pair :: Random.RandomGen g => g -> Int -> Int -> (g, (Int, Int))
pick_pair g max_i max_j = pick g xs where
    xs = [(i, j) | i <- [1..max_i], j <- [1..max_j]]

pick :: Random.RandomGen r => r -> [a] -> (r, a)
pick _ [] = error "pickFromList wrongly given empty list"
pick r as =
    let indices = (0, length as-1)
        (i, r') = Random.randomR indices r
    in  (r', as !! i)

-- Creates a list of random indices, with ranges specifies by xs
random_indices :: Random.RandomGen r => r -> [Int] -> (r, [Int])
random_indices r [] = (r, [])
random_indices r (x:xs) = (r3, (y:ys)) where
    (r2, y) = pick r [1 .. x]
    (r3, ys) = random_indices r2 xs


