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

import Csound.Base
import Csound.Catalog.Drum.Tr808
import Csound.Sam

import RandomPairs

-------------------------------------- Data -----------------------------------

{-
Notation comes from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf

Rhythms come from:
http://www.learndrumsnow.com/technique/6-simple-but-powerful-drum-beats-part-1
-}

tunes = [ 
    rhythm0,
    rhythm1,
    rhythm2,
    rhythm3,
    rhythm4,
    rhythm5,
    rhythm6,
    rhythm7,
    rhythm8,
    rhythm9,
    rhythm10,
    rhythm11,
    rhythm12,
    rhythm13,
    rhythm14,
    rhythm15,
    rhythm16,
    rhythm17,
    rhythm18,
    rhythm19
    ]

rhythm0 :: Tune
rhythm0 = Tune {
    name = "SingleBeat",
    beats = 8,
    bars = replicate 2 [(0, A), (4, A)]
    }

{-
This is the "Eighth Note Drum Beat" from:
http://www.learndrumsnow.com/technique/6-simple-but-powerful-drum-beats-part-1
-}
rhythm1 :: Tune
rhythm1 = Tune {
    name = "EighthNoteDrumBeat",
    beats = 8,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, A), (4, E), (4, HighB), (6, HighB)]
    }

{-
This is the "Super Slow Drum Beat" from:
http://www.learndrumsnow.com/technique/6-simple-but-powerful-drum-beats-part-1
-}
rhythm2 :: Tune
rhythm2 = Tune {
    name = "SuperSlowDrumBeat",
    beats = 16,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, HighB), (6, HighB), (8, E), (8, HighB), (10, HighB), (12, HighB), (14, HighB)]
    }

{-
This is the "Super Fast Drum Beat" from:
http://www.learndrumsnow.com/technique/6-simple-but-powerful-drum-beats-part-1
-}
rhythm3 :: Tune
rhythm3 = Tune {
    name = "SuperFastDrumBeat",
    beats = 8,
    bars = replicate 2 [(0, A), (0, HighB), (2, E), (2, HighB), (4, A), (4, HighB), (6, E), (6, HighB)]
    }

{-
This is the "Slow Blues Drum Beat" from:
http://www.learndrumsnow.com/technique/6-simple-but-powerful-drum-beats-part-1
-}
rhythm4 :: Tune
rhythm4 = Tune {
    name = "SlowBluesDrumBeat",
    beats = 24,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, HighB), (6, E), (6, HighB), (8, HighB), (10, A), (10, HighB), (12, A), (12, HighB), (14, HighB), (16, HighB), (18, E), (18, HighB), (20, HighB), (22, HighB)]
    }

{-
This is the "Fast Blues Shuffle Drum Beat" from:
http://www.learndrumsnow.com/technique/6-simple-but-powerful-drum-beats-part-1
-}
rhythm5 :: Tune
rhythm5 = Tune {
    name = "FastBluesShuffleDrumBeat",
    beats = 12,
    bars = replicate 3 [(0, A), (0, HighB), (4, HighB), (6, A), (6, E), (6, HighB), (10, HighB)]
    }

{-
This is the "Pop Rock Beat" from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm6 :: Tune
rhythm6 = Tune {
    name = "PopRock",
    beats = 16,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, E), (4, HighB), (6, A), (6, HighB), (8, A), (8, HighB), (10, HighB), (12, E), (12, HighB), (14, HighB)]
    }

{-
This is the "Hard Rock Beat" from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm7 :: Tune
rhythm7 = Tune {
    name = "HardRock",
    beats = 16,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, E), (4, HighB), (6, HighB), (8, A), (8, HighB), (10, A), (10, HighB), (12, E), (12, HighB), (14, HighB)]
    }

{-
This is the first half of "Stairway to Heaven" from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm8 :: Tune
rhythm8 = Tune {
    name = "StairwayToHeaven",
    beats = 16,
    bars = replicate 2 [(0, A), (0, HighB), (2, A), (2, HighB), (4, E), (4, HighB), (6, HighB), (8, A), (8, HighB), (10, A), (10, HighB), (12, E), (12, HighB), (14, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm9 :: Tune
rhythm9 = Tune {
    name = "Waltz",
    beats = 6,
    bars = replicate 4 [(0, A), (0, HighB), (2, E), (2, HighB), (4, E), (4, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm10 :: Tune
rhythm10 = Tune {
    name = "MusetteWaltz",
    beats = 6,
    bars = replicate 2 [(0, A), (0, HighB), (2, E), (4, E)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm11 :: Tune
rhythm11 = Tune {
    name = "CountryRock",
    beats = 4,
    bars = replicate 4 [(0, A), (0, HighB), (2, E), (2, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm12 :: Tune
rhythm12 = Tune {
    name = "FolkRock",
    beats = 16,
    bars = replicate 3 [(0, A), (0, HighB), (2, HighB), (4, E), (4, HighB), (6, A), (6, HighB), (8, A), (8, HighB), (10, E), (10, HighB), (12, HighB), (14, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm13 :: Tune
rhythm13 = Tune {
    name = "Samba",
    beats = 8,
    bars = replicate 2 [(0, A), (0, E), (2, E), (6, E)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm14 :: Tune
rhythm14 = Tune {
    name = "ChaChaCha",
    beats = 8,
    bars = replicate 2 [(0, A), (0, E), (0, HighB), (2, E), (2, HighB), (4, A), (4, E), (4, HighB), (5, E), (6, E), (6, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm15 :: Tune
rhythm15 = Tune {
    name = "FastReggae",
    beats = 8,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, E), (4, HighB), (6, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm16 :: Tune
rhythm16 = Tune {
    name = "Ska",
    beats = 8,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, A), (4, E), (4, HighB), (6, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm17 :: Tune
rhythm17 = Tune {
    name = "Mazurka",
    beats = 6,
    bars = replicate 4 [(0, A), (0, HighB), (2, E), (2, HighB), (4, HighB), (5, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm18 :: Tune
rhythm18 = Tune {
    name = "Twist",
    beats = 16,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, E), (4, HighB), (6, E), (6, HighB), (8, A), (8, HighB), (10, HighB), (12, E), (12, HighB), (14, HighB)]
    }

{-
This is from:
http://www.personal.kent.edu/~sbirch/Theory/21341%20CMT/Drums/Drum%20Beats%20and%20Grooves.pdf
-}
rhythm19 :: Tune
rhythm19 = Tune {
    name = "SlowWaltz",
    beats = 12,
    bars = replicate 2 [(0, A), (0, HighB), (2, HighB), (4, E), (4, HighB), (6, HighB), (8, E), (8, HighB), (10, HighB)]
    }

-------------------------------------- Types ----------------------------------

data Tune = Tune {
    name :: String,
    beats :: Int,
    bars :: [Bar]
} deriving (Eq, Ord, Show)

type Bar = [(Int, Note)]

data Note = A | E | HighB deriving (Eq, Ord)

type Loudness = Int

type Moment = Map.Map Note Loudness

instance Show Note where
    show A = "bass_drum"
    show E = "snare_drum"
    show HighB = "hi_hat"

-------------------------------------- Code -----------------------------------

output_tune :: Tune -> IO ()
output_tune t = do
    output_tune_task t TaskPrediction
    output_tune_task t TaskRetrodiction
    output_tune_task t TaskImputation

output_tune_task :: Tune -> TaskType -> IO ()
output_tune_task t task = do
    let f = "../data/rhythm/" ++ show task ++ "_" ++ name t ++ ".lp"
    putStrLn $ "Generating " ++ f
    ts <- tune_string t task   
    let s = unlines ts
    writeFile f s

tune_string :: Tune -> TaskType -> IO [String]
tune_string t task = do
    let h = "% Tune generated from " ++ name t
    let moms = moments t
    let nm = length moms
    let hms = ["", "% Sensor readings"]
    ps <- hidden_pairs task nm (Map.size (head moms))
    let ms = hms ++ concat (map (show_moment ps) (zip [1..] moms))
    let max_t = beats t * length (bars t)
    let ts = ["", "is_time(1.." ++ show max_t ++ ")."]
    return $ h : (ms ++ object_s ++ ts ++ exclusion_s)

object_s :: [String]
object_s = [
    "",
    "% Elements",
    "is_object(obj_sensor_bass_drum).",
    "is_object(obj_sensor_snare_drum).",
    "is_object(obj_sensor_hi_hat)."
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
    h j = case (t, j) `elem` ps of
        True -> "hidden"
        False -> "senses"
    f (j, (n, l)) = h j ++ "(s2(c_loudness, obj_sensor_" ++ show n ++ ", obj_loudness_" ++ show l ++ "), " ++ show t ++ ")."

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
max_loudness = 3

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
all_notes = [A, E, HighB]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["all"] -> do
            Monad.forM_ tunes output_tune
        [n] -> case find_tune n of
            Nothing -> error $ "No tune called " ++ n
            Just t -> output_tune t
        _ -> do
            putStrLn $ "Usage: rhythm <tune-name>"

find_tune :: String -> Maybe Tune
find_tune s = List.find f tunes where
    f t = name t == s

write_run_experiments :: IO ()
write_run_experiments = do
    let f = "run_rhythm_experiments.sh"
    writeFile f (unlines gen_run_experiments)
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_run_experiments :: [String] 
gen_run_experiments = h : xs where
    h = "echo \"Running rhythm experiments...\""
    xs = map f tunes
    f t = "time ./solve rhythm input_" ++ name t ++ ".lp"

write_single_experiment :: IO ()
write_single_experiment = do
    let f = "single_rhythm_experiment.sh"
    writeFile f (unlines gen_single_experiment)
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_single_experiment :: [String]
gen_single_experiment = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f (zip [1..] tunes))
    f (i, t) = ["\t" ++ show i ++ " )", "\t\techo \"Solving " ++ name t ++ "...\"", "\t\ttime ./solve rhythm input_" ++ name t ++ ".lp", "\t\t;;"]
                    
-------------------------------------- Conversion -----------------------------

data DrumSeq = DS {
    delay :: Sig,
    values :: [Sig]
}

convert_to_drum_seq :: Tune -> [(Note, DrumSeq)]
convert_to_drum_seq t = map (extract_seq t) all_notes

extract_seq :: Tune -> Note -> (Note, DrumSeq)
extract_seq t n = (n, DS { delay = d, values = vs }) where
    d = f (extract_delay t n)
    vs = map f (extract_values t n)
    f :: Int -> Sig
    f i = fromIntegral i

extract_delay :: Tune -> Note -> Int
extract_delay t n = head (get_times t n)

extract_values :: Tune -> Note -> [Int]
extract_values t n = map f (zip ts3 ts2) where
    ts = get_times t n
    ts2 = ts ++ [head ts + beats t]
    ts3 = drop 1 ts2
    f (x, y) = x - y

get_times :: Tune -> Note -> [Int]
get_times t n = List.sort $ Maybe.mapMaybe f (head (bars t)) where
    f (i, n') | n == n' = Just i
    f _ | otherwise = Nothing

play_tune :: Tune -> IO ()
play_tune t = do
    let ds = convert_to_drum_seq t
    let Just bds = lookup A ds
    let Just sns = lookup E ds
    let Just chhs = lookup HighB ds
    let bd_seq = del (delay bds) $ pat (values bds) bd
    let sn_seq = del (delay sns) $ pat (values sns) sn
    let chh_seq = del (delay chhs) $ pat (values chhs) chh
    dac $ bd_seq + sn_seq + chh_seq

write_evaluate_baselines :: IO ()
write_evaluate_baselines = do
    let f = "run_baselines_rhythm.sh"
    writeFile f (unlines evaluate_baselines)
    let c = "chmod 777 " ++ f
    Process.callCommand c

evaluate_baselines :: [String]
evaluate_baselines = xs ++ ys ++ zs where
    xs = ["echo \"Evaluating baselines for rhythm...\"", 
            "",
            "rm experiments/baselines/rhythm_*",
            ""
            ]
    ys = map f tunes
    zs = ["echo \"Correct constant baseline :\"",
            "grep \"baseline_k_rhythm_correct\" experiments/baselines/rhythm_* | wc -l",
            "echo \"Total rhythm examples\"",
            "find experiments/baselines/rhythm_* | wc -l",
            "echo \"Correct inertia baseline :\"",
            "grep \"baseline_inertia_rhythm_correct\" experiments/baselines/rhythm_* | wc -l",
            "echo \"Total rhythm examples\"",
            "find experiments/baselines/rhythm_* | wc -l"
            ]
    f t = "clingo --warn=no-atom-undefined pure/baselines.lp data_rhythm/input_" ++ name t ++ ".lp > experiments/baselines/rhythm_" ++ name t ++ ".txt"


