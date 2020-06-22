module NoisySequenceData where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Environment as Environment
import qualified System.Random as Random
import qualified System.Process as Process

data SequenceData = SequenceData {
    seq_string :: String,
    bit_vector_size :: Int,
    num_ambiguities :: Int
} deriving (Eq, Ord, Show)

data SequenceTask = SequenceTask {
    seq_data :: SequenceData,
    seq_index :: Int,
    guess_vector_size :: Int,
    guess_num_predicates :: Int
} deriving (Eq, Ord, Show)

sd :: String -> Int -> Int -> SequenceData
sd s i n = SequenceData { seq_string = s, bit_vector_size = i, num_ambiguities = n }

all_sequences :: [SequenceData]
all_sequences = [
    sd "0101010101010101010101010101010101010101" 2 1,
    sd "0101010101010101010101010101010101010101" 3 1,
    sd "0101010101010101010101010101010101010101" 4 1,
    sd "001100110011001100110011001100110011" 2 1,
    sd "001100110011001100110011001100110011" 3 1,
    sd "001100110011001100110011001100110011" 4 1,
    sd "000111000111000111000111000111000111" 2 1,
    sd "000111000111000111000111000111000111" 3 1,
    sd "000111000111000111000111000111000111" 4 1,
    sd "001001001001001001001001001001001001001" 2 1,
    sd "001001001001001001001001001001001001001" 3 1,
    sd "001001001001001001001001001001001001001" 4 1,
    sd "0001000100010001000100010001000100010001" 2 1,
    sd "0001000100010001000100010001000100010001" 3 1,
    sd "0001000100010001000100010001000100010001" 4 1,
    sd "012012012012012012012012012012012012012" 2 1,
    sd "012012012012012012012012012012012012012" 3 1,
    sd "012012012012012012012012012012012012012" 4 1
    ]

old_sequences :: [SequenceData]
old_sequences = [
    sd "0101010101010101010101010101010101010101" 3 1,
    sd "0101010101010101010101010101010101010101" 4 1,
    sd "012012012012012012012012012012012012012" 4 1,
    sd "000111000111000111000111000111000111" 3 1,
    sd "001100110011001100110011001100110011" 3 1,
    sd "0111111111111111111111111111111111111111" 3 1,
    sd "0011111111111111111111111111111111111111" 3 1,
    sd "0001111111111111111111111111111111111111" 3 1,
    sd "001001001001001001001001001001001001001" 3 1,
    sd "0001000100010001000100010001000100010001" 3 1,
    sd "0000100001000010000100001000010000100001" 3 1,
    sd "0001100011000110001100011000110001100011" 3 1,
    sd "0102010201020102010201020102010201020102" 3 1,
    sd "001002001002001002001002001002001002" 3 1,
    sd "001122001122001122001122001122001122" 3 1    
    ]

sequence_map :: [((Int, Int, Int), SequenceTask)]
sequence_map = map f all_sequence_tasks where
    f st = ((seq_index st, guess_vector_size st, guess_num_predicates st), st)

all_sequence_tasks :: [SequenceTask] 
all_sequence_tasks = concat (map make_sequence_tasks xs) where
    xs = zip [1..] all_sequences

make_sequence_tasks :: (Int, SequenceData) -> [SequenceTask]
make_sequence_tasks (i, sd) = [make_sequence_task sd i j k | j <- guess_vector_size_range sd, k <- guess_num_predicates_range sd]

guess_vector_size_range :: SequenceData -> [Int]
guess_vector_size_range sd = [pv .. nv] where
    pv = if bit_vector_size sd <= 1 then 1 else bit_vector_size sd - 1
    nv = bit_vector_size sd + 1

guess_num_predicates_range :: SequenceData -> [Int]
guess_num_predicates_range sd = [pv .. nv] where
    num_ps = num_symbols (seq_string sd)
    pv = if num_ps <= 2 then 2 else num_ps - 1
    nv = num_ps + 1

make_sequence_task :: SequenceData -> Int -> Int -> Int -> SequenceTask
make_sequence_task sd i bvs np = SequenceTask {
    seq_data = sd,
    seq_index = i,
    guess_vector_size = bvs, 
    guess_num_predicates = np
} 

num_symbols :: String -> Int
num_symbols = length . Set.toList . Set.fromList
    
is_sequence_task_guess_correct :: SequenceTask -> Bool
is_sequence_task_guess_correct st = let sd = seq_data st in
    guess_vector_size st == bit_vector_size sd && 
    guess_num_predicates st == num_symbols (seq_string sd)
