module Main where

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

import RandomPairs

-------------------------------------- Data -----------------------------------

tunes = [ 
    tune_minimal,
    tune_scale,
    tune_arpeggio,
    tune_twinkle_small,
    tune_tbm_small,
    tune_incy_wincy_spider_small,
    tune_little_lamb_small,
    tune_row_your_boat_small,
    tune_old_macdonald_small,
    tune_humpty_dumpty_small
    ]

tune_minimal :: Tune
tune_minimal = Tune {
    name = "Minimal",
    beats = 4,
    bars = [
        [(0, G)],
        [(0, E), (2, C)]
        ]
    }

tune_scale :: Tune
tune_scale = Tune {
    name = "Scale",
    beats = 4,
    bars = [
        [(0, C), (1, D), (2, E), (3, F)],
        [(0, G), (1, A), (2, B), (3, HighC)]
        ]
    }

tune_arpeggio :: Tune
tune_arpeggio = Tune {
    name = "Arpeggio",
    beats = 4,
    bars = [
        [(0, C), (1, E), (2, G), (3, HighC)]
        ]
    }

tune_twinkle_small :: Tune
tune_twinkle_small = Tune {
    name = "SmallTwinkleTwinkle",
    beats = 4,
    bars = [
        [(0, C), (1, C), (2, G), (3, G)]
        ]
    }

tune_twinkle_half :: Tune
tune_twinkle_half = Tune {
    name = "HalfTwinkleTwinkle",
    beats = 4,
    bars = [
        [(0, C), (1, C), (2, G), (3, G)],
        [(0, A), (1, A), (2, G)]
        ]
    }

tune_twinkle_slow :: Tune
tune_twinkle_slow = Tune {
    name = "SlowTwinkleTwinkle",
    beats = 4,
    bars = [
        [(0, C), (1, C), (2, G), (3, G)],
        [(0, A), (1, A), (2, G)]
        ]
    }

tune_twinkle :: Tune
tune_twinkle = Tune {
    name = "TwinkleTwinkle",
    beats = 4,
    bars = [
        [(0, C), (1, C), (2, G), (3, G)],
        [(0, A), (1, A), (2, G)],
        [(0, F), (1, F), (2, E), (3, E)],
        [(0, D), (1, D), (2, C)]
        ]
    }

tune_tbm_small :: Tune
tune_tbm_small = Tune {
    name = "ThreeBlindMiceSmall",
    beats = 6,
    bars = [
        [(0, E), (3, D)],
        [(0, C)],
        [(0, E), (3, D)],
        [(0, C)]
        ]
    }

tune_tbm :: Tune
tune_tbm = Tune {
    name = "ThreeBlindMice",
    beats = 6,
    bars = [
        [(0, E), (3, D)],
        [(0, C)],
        [(0, E), (3, D)],
        [(0, C)],
        [(0, G), (3, F), (5, F)],
        [(0, E)]
        ]
    }

tune_iggle_piggle_small :: Tune
tune_iggle_piggle_small = Tune {
    name = "IgglePiggleSmall",
    beats = 8,
    bars = [
        [(0, E), (4, G), (5, F), (6, E), (7, F)],
        [(0, E), (4, D)],
        [(0, D), (1, E), (2, F), (3, G), (4, A), (5, G), (6, F), (7, E)],
        [(0, E), (4, D)]
        ]
    }

tune_iggle_piggle :: Tune
tune_iggle_piggle = Tune {
    name = "IgglePiggle",
    beats = 8,
    bars = [
        [(0, E), (4, G), (5, F), (6, E), (7, F)],
        [(0, E), (4, D)],
        [(0, D), (1, E), (2, F), (3, G), (4, A), (5, G), (6, F), (7, E)],
        [(0, E), (4, D)],
        [(0, E), (2, C), (4, G), (5, F), (6, E), (7, F)],
        [(0, E), (4, D)],
        [(0, D), (1, E), (2, F), (3, G), (4, A), (5, G), (6, A), (7, B)],
        [(0, HighC)]
        ]
    }

tune_east_enders :: Tune
tune_east_enders = Tune {
    name = "EastEnders",
    beats = 8,
    bars = [
        [(0, E), (1, F), (2, G), (3, A), (4, B), (6, C)],
        [(0, A)],
        [(0, A), (1, G), (2, F), (3, E), (4, E), (7, E)],
        [(0, E), (1, E), (3, B), (4, E), (6, F)],
        [(0, E), (1, F), (2, G), (3, A), (4, B), (6, C)],
        [(0, A)],
        [(0, A), (1, G), (2, F), (3, E), (4, E), (7, E)],
        [(0, G), (2, A), (3, B), (4, G), (6, F)],
        [(0, C)]
        ]
    }

tune_incy_wincy_spider_small :: Tune
tune_incy_wincy_spider_small = Tune {
    name = "IncyWincySpiderSmall",
    beats = 3,
    bars = [
        [(0, C), (2, C)],
        [(0, C), (2, D)],
        [(0, E)],
        [(0, E), (2, E)],
        [(0, D), (2, C)]
        ]
    }

tune_incy_wincy_spider :: Tune
tune_incy_wincy_spider = Tune {
    name = "IncyWincySpider",
    beats = 3,
    bars = [
        [(0, C), (2, C)],
        [(0, C), (2, D)],
        [(0, E)],
        [(0, E), (2, E)],
        [(0, D), (2, C)],
        [(0, D), (2, E)],
        [(0, C)]
        ]
    }

tune_little_lamb_small :: Tune
tune_little_lamb_small = Tune {
    name = "LittleLambSmall",
    beats = 8,
    bars = [
        [(0, E), (3, D), (4, C), (6, D)],
        [(0, E), (2, E), (4, E)],
        [(0, D), (2, D), (4, D)],
        [(0, E), (2, G), (4, G)]
        ]
    }

tune_little_lamb :: Tune
tune_little_lamb = Tune {
    name = "LittleLamb",
    beats = 8,
    bars = [
        [(0, E), (3, D), (4, C), (6, D)],
        [(0, E), (2, E), (4, E)],
        [(0, D), (2, D), (4, D)],
        [(0, E), (2, G), (4, G)],
        [(0, E), (3, D), (4, C), (6, D)],
        [(0, E), (2, E), (4, E), (6, E)],
        [(0, D), (2, D), (4, E), (6, D)],
        [(0, C)]
        ]
    }

tune_row_your_boat_small :: Tune
tune_row_your_boat_small = Tune {
    name = "RowYourBoatSmall",
    beats = 6,
    bars = [
        [(0, C), (3, C)],
        [(0, C), (2, D), (3, E)],
        [(0, E), (2, D), (3, E), (5, F)],
        [(0, G)]
        ]
    }

tune_row_your_boat :: Tune
tune_row_your_boat = Tune {
    name = "RowYourBoat",
    beats = 6,
    bars = [
        [(0, C), (3, C)],
        [(0, C), (2, D), (3, E)],
        [(0, E), (2, D), (3, E), (5, F)],
        [(0, G)],
        [(0, HighC), (1, HighC), (2, HighC), (3, G), (4, G), (5, G)],
        [(0, E), (1, E), (2, E), (3, C), (4, C), (5, C)],
        [(0, G), (2, F), (3, E), (5, D)],
        [(0, C)]
        ]
    }

tune_old_macdonald_small :: Tune
tune_old_macdonald_small = Tune {
    name = "OldMacdonaldSmall",
    beats = 4,
    bars = [
        [(0, F), (1, F), (2, F), (3, C)],
        [(0, D), (1, D), (2, C)],
        [(0, A), (1, A), (2, G), (3, G)],
        [(0, F)]
        ]
    }

tune_old_macdonald :: Tune
tune_old_macdonald = Tune {
    name = "OldMacdonald",
    beats = 4,
    bars = [
        [(0, F), (1, F), (2, F), (3, C)],
        [(0, D), (1, D), (2, C)],
        [(0, A), (1, A), (2, G), (3, G)],
        [(0, F), (3, C)],
        [(0, F), (1, F), (2, F), (3, C)],
        [(0, D), (1, D), (2, C)],
        [(0, A), (1, A), (2, G), (3, G)],
        [(0, F)]
        ]
    }

tune_humpty_dumpty_small :: Tune
tune_humpty_dumpty_small = Tune {
    name = "HumptyDumptySmall",
    beats = 3,
    bars = [
        [(0, C), (2, C)],
        [(0, E), (2, E)],
        [(0, D), (1, E), (2, D)],
        [(0, C)],
        [(0, E), (2, E)],
        [(0, G), (2, G)]
        ]        
    }

tune_humpty_dumpty :: Tune
tune_humpty_dumpty = Tune {
    name = "HumptyDumpty",
    beats = 3,
    bars = [
        [(0, C), (2, C)],
        [(0, E), (2, E)],
        [(0, D), (1, E), (2, D)],
        [(0, C)],
        [(0, E), (2, E)],
        [(0, G), (2, G)],
        [(0, F), (1, G), (2, F)],
        [(0, E)]
        ]        
    }

-------------------------------------- Types ----------------------------------

data Tune = Tune {
    name :: String,
    beats :: Int,
    bars :: [Bar]
} deriving (Eq, Ord, Show)

type Bar = [(Int, Note)]

data Note = C | D | E | F | G | A | B | HighC deriving (Eq, Ord)

type Loudness = Int

type Moment = Map.Map Note Loudness

instance Show Note where
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"
    show G = "g"
    show A = "a"
    show B = "b"
    show HighC = "high_c"

-------------------------------------- Code -----------------------------------

output_tune :: Tune -> IO ()
output_tune t = do
    output_tune_task t TaskPrediction
    output_tune_task t TaskRetrodiction
    output_tune_task t TaskImputation

output_tune_task :: Tune -> TaskType -> IO ()
output_tune_task t task = do
    let f = "../data/music/" ++ show task ++ "_" ++ name t ++ ".lp"
    putStrLn $ "Generating " ++ f
    ts <- tune_string t task
    let s = unlines ts
    writeFile f s

tune_string :: Tune -> TaskType -> IO [String]
tune_string t task = do
    let h = "% Tune generated from " ++ name t
    -- We cut off the last moments to make the problem less trivial.
    -- In lots of the tunes, the last few moments are all 0s.
    let num_dropped = num_moments_to_drop t
    let moms = take (length (moments t) - num_dropped) (moments t)
    let nm = length moms
    let hms = ["", "% Sensor readings"]
    ps <- hidden_pairs task nm (Map.size (head moms))
    let ms = hms ++ concat (map (show_moment ps) (zip [1..] moms))
    let max_t = beats t * length (bars t) - num_dropped
    let ts = ["", "is_time(1.." ++ show max_t ++ ")."]
    return $ h : (ms ++ object_s ++ ts ++ exclusion_s)

num_moments_to_drop :: Tune -> Int
num_moments_to_drop t = result where
    b = last (bars t)
    (n, _) = last b
    result = beats t - (n + 2)

object_s :: [String]
object_s = [
    "",
    "% Elements",
    "is_object(obj_sensor_c).",
    "is_object(obj_sensor_d).",
    "is_object(obj_sensor_e).",
    "is_object(obj_sensor_f).",
    "is_object(obj_sensor_g).",
    "is_object(obj_sensor_a).",
    "is_object(obj_sensor_b).",
    "is_object(obj_sensor_high_c)."
    ]

exclusion_s :: [String]
exclusion_s = [
    "",
    "% ∃! clause for c_loudness : at most one",
    ":-",
    "\tholds(s2(c_loudness, X, Y), T),",
    "\tholds(s2(c_loudness, X, Y2), T),",
    "\tY != Y2.",
    "",
    "% ∃! clause for c_loudness : at least one",
    ":-",
    "\tpermanent(isa(t_sensor, X)),",
    "\tis_time(T),",
    "\tnot aux_c_loudness(X, T).",
    "",
    "aux_c_loudness(X, T) :-",
    "\tholds(s2(c_loudness, X, _), T).",
    "",
    "% Incompossibility for p_r",
    "incompossible(s2(c_loudness, X, Y), s2(c_loudness, X, Y2)) :-",
    "\tpermanent(isa(t_sensor, X)),",
    "\tpermanent(isa(t_loudness, Y)),",
    "\tpermanent(isa(t_loudness, Y2)),",
    "\tY != Y2."
    ]

show_moment :: [(Int, Int)] -> (Int, Moment) -> [String]
show_moment ps (t, m) = map f (zip [1..] (Map.toList m)) where
    f (j, (n, l)) = case (t,j) `elem` ps of
        False -> "senses(s2(c_loudness, obj_sensor_" ++ show n ++ ", obj_loudness_" ++ show l ++ "), " ++ show t ++ ")."
        True -> "hidden(s2(c_loudness, obj_sensor_" ++ show n ++ ", obj_loudness_" ++ show l ++ "), " ++ show t ++ ")."

moments :: Tune -> [Moment]
moments t = fixed_point apply_decay (concat (map (moment t) (bars t)))

fixed_point :: Eq a => (a->a) -> a -> a
fixed_point f a = 
    if f a == a then a
    else fixed_point f (f a)

moment :: Tune -> Bar -> [Moment]
moment t bar = List.foldl' (interpret_note t) b bar where
    b = replicate (beats t) initial_moment

interpret_note :: Tune -> [Moment] -> (Int, Note) -> [Moment]
interpret_note t ms (begin, n) = ms1 ++ ms2 ++ ms3 where
    ms1 = take begin ms
    ms2 = [update_note n (ms !! begin)]
    ms3 = drop (begin + 1) ms

update_note :: Note -> Moment -> Moment
update_note n m = Map.insert n max_loudness m

max_loudness :: Int
max_loudness = 2

initial_moment :: Moment
initial_moment = Map.fromList [(n, 0) | n <- all_notes]

apply_decay :: [Moment] -> [Moment]
apply_decay [] = []
apply_decay (m : ms) = m : apply_decay2 ms m

apply_decay2 :: [Moment] -> Moment -> [Moment]
apply_decay2 [] _ = []
apply_decay2 (m : ms) prev = m' : ms' where
    ms' = apply_decay2 ms m
    m' = decay m prev

decay :: Moment -> Moment -> Moment
decay now prev = List.foldl' f now all_notes where
    f m n = case (Map.lookup n m, Map.lookup n prev) of
        (Just l, Just l') -> case l' > l of
            False -> m
            True -> Map.insert n (l'-1) m

all_notes :: [Note]
all_notes = [C, D, E, F, G, A, B, HighC]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["all"] -> do
            Monad.forM_ tunes output_tune
            putStrLn "Also generating run_music_experiments.sh"
        [n] -> case find_tune n of
            Nothing -> error $ "No tune called " ++ n
            Just t -> output_tune t
        _ -> do
            putStrLn $ "Usage: music <tune-name>"

find_tune :: String -> Maybe Tune
find_tune s = List.find f tunes where
    f t = name t == s

write_run_experiments :: IO ()
write_run_experiments = do
    let f = "run_music_experiments.sh"
    writeFile f (unlines gen_run_experiments)
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_run_experiments :: [String] 
gen_run_experiments = h : xs where
    h = "echo \"Running music experiments...\""
    xs = map f tunes
    f t = "time ./solve music input_" ++ name t ++ ".lp"

write_single_experiment :: IO ()
write_single_experiment = do
    let f = "single_music_experiment.sh"
    writeFile f (unlines gen_single_experiment)
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_single_experiment :: [String]
gen_single_experiment = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f (zip [1..] tunes))
    f (i, t) = ["\t" ++ show i ++ " )", "\t\techo \"Solving " ++ name t ++ "...\"", "\t\ttime ./solve music input_" ++ name t ++ ".lp", "\t\t;;"]
                    
write_evaluate_baselines :: IO ()
write_evaluate_baselines = do
    let f = "run_baselines_music.sh"
    writeFile f (unlines evaluate_baselines)
    let c = "chmod 777 " ++ f
    Process.callCommand c

evaluate_baselines :: [String]
evaluate_baselines = xs ++ ys ++ zs where
    xs = ["echo \"Evaluating baselines for music...\"", 
            "",
            "rm experiments/baselines/music_*",
            ""
            ]
    ys = map f tunes
    zs = ["echo \"Correct constant baseline :\"",
            "grep \"baseline_k_music_correct\" experiments/baselines/music_* | wc -l",
            "echo \"Total music examples\"",
            "find experiments/baselines/music_* | wc -l",
            "echo \"Correct inertia baseline :\"",
            "grep \"baseline_inertia_music_correct\" experiments/baselines/music_* | wc -l",
            "echo \"Total music examples\"",
            "find experiments/baselines/music_* | wc -l"
            ]
    f t = "clingo --warn=no-atom-undefined pure/baselines.lp data/music/input_" ++ name t ++ ".lp > experiments/baselines/music_" ++ name t ++ ".txt"


                        
