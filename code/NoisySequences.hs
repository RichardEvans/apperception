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

import NoisySequenceData

type BitVector = [Bool]


-------------------------------------------------------------------------------
-- There are many possible bit vectors for each numeric symbol.
--
-- So, for example, if there are 2 symbols and a bit vector of length 3,
-- then there are 2^3 bit vectors, and there are 2^3/2 bit vectors for
-- each symbol: 
--  0 ↦ [0,0,0], [0,0,1], [0,1,0], [0,1,1]
--  1 ↦ [1,0,0], [1,0,1], [1,1,0], [1,1,1]
--
-- But we also need to have some ambiguous bit vectors that are used
-- for multiple symbols.
-- We create ambiguity in a simple way by reusing one of the bit vectors
-- of symbol i+1 for symbol i.
--
-- So, for example, we create ambiguity by adding [0,1,1] to 1's bit vectors,
-- to create:
--  0 ↦ [0,0,0], [0,0,1], [0,1,0], [0,1,1]
--  1 ↦ [0,1,1], [1,0,0], [1,0,1], [1,1,0], [1,1,1]
--
-- Here, [0,1,1] is the one ambiguous entry that is used for both symbols.
-------------------------------------------------------------------------------

symbol_index_map :: Random.RandomGen g => g -> SequenceData -> (g, [(Int, Int)])
symbol_index_map g sd = (g', res) where
    num_bits = bit_vector_size sd :: Int
    num_bit_vectors = 2 ^ num_bits :: Int
    num_vectors_per_symbol = num_bit_vectors `div` num_symbols s :: Int
    s = seq_string sd
    (res, g') = List.foldl' f ([], g) [0 .. length s - 1]
    f (assocs, r) i = (assocs ++ [(x,j)], r') where
        x = read [((seq_string sd) !! i)] :: Int
        (r', j) = pick r [start .. end]
        start = x * num_vectors_per_symbol
        end = start + num_vectors_per_symbol - 1

make_symbol_index_map_ambiguous :: Random.RandomGen g => SequenceData -> g -> [(Int, Int)] -> (g, [(Int, Int)])
make_symbol_index_map_ambiguous sd g assocs = (g', assocs') where
    -- we do -2 because we don't want to ambiguate the last (held-out) timestep
    ns = [0 .. (length assocs - 2)] :: [Int]
    (g', index) = pick_such_that f g ns
    f i = let (x, _) = assocs !! i in x > 0
    (x, _) = assocs !! index
    y' = x * num_vectors_per_symbol - 1
    num_bits = bit_vector_size sd :: Int
    num_bit_vectors = 2 ^ num_bits :: Int
    num_vectors_per_symbol = num_bit_vectors `div` num_symbols s :: Int
    s = seq_string sd
    assocs' = insert_at (x, y') index assocs

ambiguous_symbol_index_map :: Random.RandomGen g => g -> SequenceData -> (g, [(Int, Int)])
ambiguous_symbol_index_map g sd = List.foldl' f (g2, assocs) [1 .. n] where
    (g2, assocs) = symbol_index_map g sd
    f (g3, assocs') _ = make_symbol_index_map_ambiguous sd g3 assocs'
    n = num_ambiguities sd

gen_noisy_sequence :: SequenceData -> IO (BitVector, [(Int, Int)])
gen_noisy_sequence sd = do
    g <- Random.newStdGen
    let (_, assocs) = ambiguous_symbol_index_map g sd
    -- putStrLn $ "Generating noisy sequence for: " ++ seq_string sd
    -- putStrLn $ "with " ++ show (num_ambiguities sd) ++ " ambiguous entries"
    -- print assocs
    let bv = assocs_to_bit_vector sd assocs
    return (bv, assocs)

assocs_to_bit_vector ::  SequenceData -> [(Int, Int)] -> BitVector
assocs_to_bit_vector sd assocs = bv where
    num_bits = bit_vector_size sd
    num_bit_vectors = 2 ^ num_bits
    num_syms = num_symbols (seq_string sd)
    num_vectors_per_symbol = num_bit_vectors `div` num_syms
    g x = replicate (num_bits - length (toBin x)) False ++ toBin x
    bv = List.foldl' f [] (map snd assocs)
    f x i = x ++ g i

insert_at :: a -> Int -> [a] -> [a]
insert_at x i xs = take i xs ++ [x] ++ drop (i+1) xs

pick_such_that :: Random.RandomGen r => (a -> Bool) -> r -> [a] -> (r, a)
pick_such_that _ _ [] = error "pickFromList wrongly given empty list"
pick_such_that f r as =
    let indices = (0, length as-1)
        (i, r') = Random.randomR indices r
    in  case f (as !! i) of
        True -> (r', as !! i)
        False -> pick_such_that f r' as

toBin :: Int -> [Bool]
toBin 0 = [False]
toBin 1 = [True]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [False]
    | otherwise = toBin (n `div` 2) ++ [True]    

pick :: Random.RandomGen r => r -> [a] -> (r, a)
pick _ [] = error "pickFromList wrongly given empty list"
pick r as =
    let indices = (0, length as-1)
        (i, r') = Random.randomR indices r
    in  (r', as !! i)

convert_to_index_list :: [[Bool]] -> [[Bool]] -> [Int]
convert_to_index_list bv_list bvs = List.foldl' f [] bv_list where
    f acc bv = let Just i = List.elemIndex bv bvs in acc ++ [i]

split_bit_vector :: [a] -> Int -> [[a]]
split_bit_vector xs n = r' where
    r = Split.chunksOf n xs
    x = last r
    r' = case length x == n of
        True -> r
        False -> take (length r - 1) r

gen_sequence_task :: SequenceTask -> IO [String]
gen_sequence_task st = do
    (bv, assocs) <- gen_noisy_sequence (seq_data st)
    return (sequence_task st bv assocs)

write_sequence_task :: SequenceTask -> IO ()
write_sequence_task st = do
    ss <- gen_sequence_task st
    let output_f = "data/noisy/" ++ sequence_task_file_name st
    putStrLn $ "Creating " ++ output_f
    writeFile output_f (unlines ss)

sequence_task_file_name :: SequenceTask -> String
sequence_task_file_name st = "predict_" ++ show (seq_index st) ++ "_" ++ show (guess_vector_size st) ++ "_" ++ show (guess_num_predicates st) ++ ".lp"

sequence_task :: SequenceTask -> BitVector -> [(Int, Int)] -> [String] 
sequence_task st bv assocs = sequence_task_comments st bv ++ sequence_task_bnn_input st bv ++ sequence_task_bv_at st bv assocs ++ sequence_elements st bv ++ sequence_concepts st ++ sequence_bnn st ++ possible_pred st ++ senses_choice ++ use_every_predicate st ++ weak_constraints

weak_constraints = [
    "", 
    --":~ possible_pred(BV, C). [1 @ 3, BV, C]",
    "",
    "is_possible_pred(C) :- possible_pred(_, C).",
    "",
    "count_bnn_examples_per_predicate(C, N) :-",
    "\tis_possible_pred(C),",
    "\tN = #count { E : possible_pred(E, C) }.",
    "",
    "num_bvs(N) :- N = #count { BV : bv_at(T, BV), is_time(T) }.",
    "",
    "max_bnn_examples_per_predicate(M) :-",
    "\tM = #max { N : count_bnn_examples_per_predicate(C, N) }.",
    "",
    ":~ max_bnn_examples_per_predicate(M). [ M @ 2, M]"
    ]
    
use_every_predicate :: SequenceTask -> [String]
use_every_predicate _ = []
--use_every_predicate st = "" : "% Insist that every visual predicate is used at least once" : h : rs where
--    rs = map f [1..guess_num_predicates st]
--    f i = ":- not is_predicate_used(c_" ++ show i ++ ")."
--    h = "is_predicate_used(C) :- senses(s(C, X), T)."

senses_choice :: [String]
senses_choice = ["", "% Senses choice from bnn assignments", c] where
    c = "1 { senses(s(C, obj_sensor), T) : possible_pred(BV, C) } 1 :- bv_at(T, BV)."

possible_pred :: SequenceTask -> [String]
possible_pred st = "" : "% Possible predicate assignments from bnn" : rs where
    rs = map f [1 .. guess_num_predicates st]
    f i = "possible_pred(BV, c_" ++ show i ++ ") :- bnn_result(BV, " ++ show i ++ ", 1)."

sequence_bnn :: SequenceTask -> [String]
sequence_bnn st = "" : "% Binary neural network" : l1 : l2 : l3 : [] where
    l1 = "nodes(1, " ++ show (guess_vector_size st) ++ ")."
    l2 = "nodes(2, " ++ show (guess_num_predicates st) ++ ")."
    l3 = "nodes(3, " ++ show (guess_num_predicates st) ++ ")."

sequence_concepts :: SequenceTask -> [String]
sequence_concepts st = "" : h : cs ++ xs where
    h = "% Concepts"
    m = guess_num_predicates st
    cs = map f [1 .. m]
    f i = "is_concept(c_" ++ show i ++ ")."
    xs = ("" : at_mosts m) ++ ("" : at_leasts m) ++ ("" : incompossibles m)

at_mosts :: Int -> [String]
at_mosts m = "% At most one" : map f xs where
    xs = [(i, j) | i <- [1..m], j <- [1..m], i < j]
    f (i, j) = ":- holds(s(c_" ++ show i ++ ", X), T), holds(s(c_" ++ show j ++ ", X), T)."

at_leasts :: Int -> [String]
at_leasts m = ["% At least one", ":-", "\tpermanent(isa(t_sensor, X)),", 
    "\tis_time(T),"] ++ map f [1..m] where
        f i = "\tnot holds(s(c_" ++ show i ++ ", X), T)" ++ g i
        g i | i >= m = "."
        g _ | otherwise = ","

incompossibles :: Int -> [String]
incompossibles m = "% Incompossibility" : map f xs where
    xs = [(i, j) | i <- [1..m], j <- [1..m], i < j]
    f (i, j) = "incompossible(s(c_" ++ show i ++ ", X), s(c_" ++ show j ++ ", X)) :- permanent(isa(t_sensor, X))."

sequence_elements :: SequenceTask -> BitVector -> [String] 
sequence_elements st bv = "" : header : obj : time : [] where
    header = "% Elements"
    obj = "is_object(obj_sensor)."
    time = "is_time(1.." ++ show (time_steps bv h) ++ ")."
    h = guess_vector_size st

time_steps :: BitVector -> Int -> Int    
time_steps bv h = length bv_list where
    bv_list = split_bit_vector bv h

sequence_task_bnn_input :: SequenceTask -> BitVector -> [String] 
sequence_task_bnn_input st bv = "":header:rs where
    sd = seq_data st
    header = "% Inputs to binary neural network"
    rs = concat (map f (zip [0..] bvs))
    bv_list = split_bit_vector bv h
    h = guess_vector_size st
    bvs = Set.toList (Set.fromList bv_list)
    f (i, bv) = map (g i) (zip [1..] bv)
    g i (j, x) = "bnn_input(ex_" ++ show i ++ ", node(1, " ++ show j ++ "), " ++ show_bv [x] ++ ")."

num_held_outs :: SequenceTask -> Int
num_held_outs st = case guess_vector_size st of
    5 -> 2
    4 -> 3
    3 -> 3
    2 -> 3
    _ -> 3

sequence_task_bv_at :: SequenceTask -> BitVector -> [(Int, Int)] -> [String] 
sequence_task_bv_at st bv assocs = "":header:rs where
    sd = seq_data st
    header = "% Sequence"
    n = num_held_outs st
    rs = map f (zip [1..length bv_index_list - n] bv_index_list) ++ hos
    bv_list = split_bit_vector bv h
    h = guess_vector_size st
    bvs = Set.toList (Set.fromList bv_list)
    bv_index_list = convert_to_index_list bv_list bvs
    f (t, i) = "bv_at(" ++ show t ++ ", ex_" ++ show i ++ ")."
    hos = hold_outs st bv assocs n

sequence_task_comments :: SequenceTask -> BitVector -> [String] 
sequence_task_comments st bv = [header, s, t, s, o, s, b, s, a, s, bvs, s, ivs, s, gnps, s, correct, s] ++ ss ++ [header] where
    sd = seq_data st
    header = "%--------------------------------------------------"
    s = "%"
    t = "% Generated by NoisySequences.hs"
    o = "% Original sequence: " ++ seq_string sd
    b = "% True bit vector size: " ++ show (bit_vector_size sd)
    a = "% Num ambiguities: " ++ show (num_ambiguities sd)
    bvs = "% Bit vector: " ++ show_bv (take 60 bv) ++ "..."
    ivs = "% Guessed bit vector size: " ++ show h
    gnps = "% Guessed num predicates: " ++ show (guess_num_predicates st)
    ss = "% Sequence:" : map f (zip [1..] bv_list)
    correct = if is_sequence_task_guess_correct st then "% (Guesses are correct)" else "% (Guesses are wrong)"
    f (t, bv) = "% Time " ++ show t ++ ": " ++ show_bv bv
    h = guess_vector_size st
    bv_list = split_bit_vector bv h

show_bv :: BitVector -> String
show_bv bv = map f bv where
    f True = '1'
    f False = '0'

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> gen_all
        ["all"] -> gen_all
        _ -> error "Unexpected usage"

gen_all :: IO ()
gen_all = do
    Monad.forM_ all_sequence_tasks write_sequence_task
    write_single_experiment

write_single_experiment :: IO ()
write_single_experiment = do
    let f = "scripts/single_noisy_sequence.sh"
    writeFile f (unlines (gen_single_experiment (zip [1..] all_sequence_tasks)))
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_single_experiment :: [(Int, SequenceTask)] -> [String]
gen_single_experiment seqs = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f seqs)
    f (n, st) = [
        "\t" ++ show n ++ " )", 
        "\t\techo \"Solving " ++ sequence_task_file_name st ++ "\"", 
        "\t\ttime code/solve noisy " ++ show (seq_index st) ++ " " ++ show (guess_vector_size st) ++ " " ++ show (guess_num_predicates st), "\t\t;;"
        ]

-------------------------------------------------------------------------------

test_accuracy :: Int -> Int -> IO ()
test_accuracy i n = do
    let st = all_sequence_tasks !! i
    print st
    (bv, assocs) <- gen_noisy_sequence (seq_data st)

    let (as', us', ambs') = held_outs2 st bv assocs n
    putStrLn "Original acceptables:"
    Monad.forM_ as' print
    putStrLn "Original unacceptables:"
    Monad.forM_ us' print
    putStrLn "Original ambiguities:"
    Monad.forM_ ambs' print

    let actual_size = bit_vector_size (seq_data st)
    let as2 = List.nub $ map (convert_to_bit_vector actual_size) as'
    let us2 = List.nub $ map (convert_to_bit_vector actual_size) us'
    let ambs2 = List.nub $ map (convert_to_bit_vector actual_size) ambs'
    putStrLn "Vector of Original acceptables:"
    Monad.forM_ as2 print
    putStrLn "Vector of Original unacceptables:"
    Monad.forM_ us2 print
    putStrLn "Vector of Original ambiguities:"
    Monad.forM_ ambs2 print

    let (as, us) = held_outs st bv assocs n
    putStrLn "Acceptables:"
    Monad.forM_ as print
    putStrLn "Unacceptables:"
    Monad.forM_ us print
    putStrLn "Map:"
    let m = vector_index_map st bv
    print m
    let as2 = Maybe.mapMaybe (convert_bv_to_guess_indices st bv) as
    let us2 = Maybe.mapMaybe (convert_bv_to_guess_indices st bv) us
    putStrLn "Converted acceptables:"
    Monad.forM_ as2 print
    putStrLn "Converted unacceptables:"
    Monad.forM_ us2 print
    putStrLn "Code:"
    let ss = hold_outs st bv assocs n
    Monad.forM_ ss putStrLn
    let t = length bv `quot` guess_vector_size st
    let m2 = index_vector_map st bv
    let ss = check_acceptables st m2 t as2
    let ss2 = check_unacceptables m2 t us2
    putStrLn "Checking acceptables:"
    Monad.forM_ ss putStrLn
    putStrLn "Checking unacceptables:"
    Monad.forM_ ss2 putStrLn

check_acceptables :: SequenceTask -> Map.Map Int BitVector -> Int -> [[Int]] -> [String]
check_acceptables _ _ _ as | null as = []
check_acceptables st m t as = xs ++ (List.sort (List.nub (concat (map f as)))) where
    f a = concat $ map g (zip [1..] a)
    g (i, x) = ["incorrect :- holds(s(P, obj_sensor), " ++ show (t - length (head as) + i) ++ "), is_visual_pred(P), not possible_pred(ex_" ++ show x ++ ", P).", let Just b = Map.lookup x m in "% acceptable at (" ++ show (t - length (head as) + i) ++ "): " ++ show b]
    xs = map h [1..guess_num_predicates st]
    h i = "is_visual_pred(c_" ++ show i ++ ")."

check_unacceptables :: Map.Map Int BitVector -> Int -> [[Int]] -> [String]
check_unacceptables _ _ us | null us = []
check_unacceptables m t us = concat (map f us) ++ map p us where
    f a = "incorrect :-" : holds ++ possibles a
    holds = map g [1..n]
    n = length (head us)
    g i = "\tholds(s(P" ++ show i ++ ", obj_sensor), " ++ show (t - n + i) ++ "),"
    possibles a = map h (zip [1..n] a)
    h (i, x) = "\tpossible_pred(ex_" ++ show x ++ ", P" ++ show i ++ ")" ++ if i == n then "." else ","
    p u = "% unacceptable: " ++ concat (map q (zip [1..n] u))
    q (i, x) = "(" ++ show (t - n + i) ++ "): " ++ let Just b = Map.lookup x m in show b ++ " "

convert_bv_to_guess_indices :: SequenceTask -> BitVector -> BitVector -> Maybe [Int]
convert_bv_to_guess_indices st bv xs = List.foldl' f (Just []) xs2 where
    xs2 = chop_list xs (guess_vector_size st)
    f Nothing _ = Nothing
    f (Just acc) x = case Map.lookup x m of
        Nothing -> Nothing
        Just i -> Just (acc ++ [i])
    m = vector_index_map st bv

chop_list :: [a] -> Int -> [[a]]
chop_list xs n = Split.chunksOf n xs

index_vector_map :: SequenceTask -> BitVector -> Map.Map Int BitVector
index_vector_map st bv = Map.fromList zs where
    bv_list = split_bit_vector bv h
    h = guess_vector_size st
    bvs = Set.toList (Set.fromList bv_list)
    zs = zip [0..] bvs 

vector_index_map :: SequenceTask -> BitVector -> Map.Map BitVector Int
vector_index_map st bv = Map.fromList zs where
    bv_list = split_bit_vector bv h
    h = guess_vector_size st
    bvs = Set.toList (Set.fromList bv_list)
    zs = zip bvs [0..] 

type BitVectorIndex = Int

type BitVectorIndices = [BitVectorIndex]

hold_outs :: SequenceTask -> BitVector -> [(Int, Int)] -> Int -> [String]     
hold_outs st bv assocs n = "" : ss ++ ss2 where
    (as, us) = held_outs st bv assocs n
    m = vector_index_map st bv
    as2 = Maybe.mapMaybe (convert_bv_to_guess_indices st bv) as
    us2 = Maybe.mapMaybe (convert_bv_to_guess_indices st bv) us 
    t = length bv `quot` guess_vector_size st 
    m2 = index_vector_map st bv  
    ss = check_acceptables st m2 t as2
    ss2 = check_unacceptables m2 t us2

held_outs :: SequenceTask -> BitVector -> [(Int, Int)] -> Int -> ([BitVector], [BitVector])
held_outs st bv assocs n = (as5, us5) where
    (as, us, ambs) = held_outs2 st bv assocs n
    actual_size = bit_vector_size (seq_data st)
    as2 = List.nub $ map (convert_to_bit_vector actual_size) as
    us2 = List.nub $ map (convert_to_bit_vector actual_size) us
    ambs2 = List.nub $ map (convert_to_bit_vector actual_size) ambs
    guessed_vector_length = n * guess_vector_size st
    as3 = List.nub $ map (take_last guessed_vector_length) as2
    us3 = List.nub $ map (take_last guessed_vector_length) us2
    ambs3 = List.nub $ map (take_last guessed_vector_length) ambs2  
    as4 = List.nub $ as3 List.\\ ambs3  
    us4 = List.nub $ us3 List.\\ ambs3  
    as5 = as4 List.\\ us4
    us5 = us4 List.\\ as4

convert_to_bit_vector :: Int -> BitVectorIndices -> BitVector
convert_to_bit_vector num_bits bvis = List.foldl' f [] bvis where
    g x = replicate (num_bits - length (toBin x)) False ++ toBin x
    f x i = x ++ g i

take_last :: Int -> [a] -> [a]
take_last n xs = drop (length xs - n) xs

-- Given a sequence task, a bit vector, an assocs list, 
-- and a number n of held out vectors,
-- produces a list of acceptable and unacceptable bit vector indices,
-- plus a list of ambiguous bit vector indices.
-- 
-- NB these bit vector indices are indices into the 
-- actual (correct) bit-vectors, not the guessed bit vectors!
held_outs2 :: SequenceTask -> BitVector -> [(Int, Int)] -> Int -> ([BitVectorIndices], [BitVectorIndices], [BitVectorIndices])
held_outs2 st bv assocs n = (acceptables, unacceptables, ambs) where
    k = num_actual_vectors st n
    all_finals = all_final_vectors st m k
    ambiguities = ambiguous_indices m
    acceptables = setify $ filter (is_unambiguous ambiguities) all_finals
    unacceptables = setify $ filter (is_unambiguous ambiguities) all_others
    ambs = setify $ filter (\x -> not (is_unambiguous ambiguities x)) (all_vectors assocs k)
    m = extract_symbol_indices_map assocs
    all_others = (all_vectors assocs k) List.\\ all_finals
    setify = List.sort . List.nub 

all_vectors :: [(Int, Int)] -> Int -> [BitVectorIndices]    
all_vectors _ 0 = [[]]
all_vectors assocs n = [x : xs | x <- as, xs <- all_vectors assocs (n-1)] where as = List.nub (map snd assocs)

is_unambiguous :: [BitVectorIndex] -> BitVectorIndices -> Bool
is_unambiguous ambs bvis = all p bvis where
    p bvi = not (bvi `elem` ambs)

-- If G is guessed vector size, A is actual vector size, 
-- and N is number of guessed vectors,
-- then we want the smallest K such that K * A >= G * N.
num_actual_vectors :: SequenceTask -> Int -> Int
num_actual_vectors st n = max 1 k where
    g = guess_vector_size st
    x = g * n
    a = bit_vector_size (seq_data st)
    d = x `quot` a
    r = case x `rem` a of
        0 -> 0
        _ -> 1
    k = d + r

type SymbolIndicesMap = Map.Map Int [Int]

ambiguous_indices :: SymbolIndicesMap -> [BitVectorIndex]
ambiguous_indices m = List.nub [ i | (s, is) <- m', i <- is, (s2, is2) <- m', s /= s2, i `elem` is2] where m' = Map.assocs m

all_indices :: SymbolIndicesMap -> [BitVectorIndex]
all_indices m = List.nub $ concat [ is | (_, is) <- m'] where m' = Map.assocs m

-- Generate all bit vectors for the last n elements of the sequence
all_final_vectors :: SequenceTask -> SymbolIndicesMap -> Int -> [BitVectorIndices]
all_final_vectors st m k = List.nub $ all_final_vectors2 st m xs where
    xs = drop (length s - k) s
    s = seq_string (seq_data st)

all_final_vectors2 :: SequenceTask -> SymbolIndicesMap -> String -> [BitVectorIndices]
all_final_vectors2 _ _ "" = [[]]
all_final_vectors2 st m (s:ss) = [a : b | a <- all_bit_vectors s st m, b <- all_final_vectors2 st m ss]

all_bit_vectors :: Char -> SequenceTask -> SymbolIndicesMap -> [BitVectorIndex]
all_bit_vectors c st m = xs where
    i = read [c] :: Int
    Just xs = Map.lookup i m

extract_symbol_indices_map :: [(Int, Int)] -> SymbolIndicesMap    
extract_symbol_indices_map assocs = List.foldl' f Map.empty assocs where
    f m (i, j) = case Map.lookup i m of
        Nothing -> Map.insert i [j] m
        Just js -> Map.insert i (j : js) m

