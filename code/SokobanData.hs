module SokobanData where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Environment as Environment
import qualified System.Process as Process
import qualified System.Random as Random

import SokobanTypes

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

sokoban_examples :: [(String, Example)]
sokoban_examples = concat (map make_n_examples (zip [1..] examples))

examples :: [Example]
examples = [example_1, example_2, example_3, example_4, example_5, example_6, example_7, example_8, example_9, example_10]

make_n_examples :: (Int, Example) -> [(String, Example)]
make_n_examples (n, ex) = map f xs where
    xs = [3,5,7,9,11,13,15,17]
    f x = ("e_" ++ show n ++ "_" ++ show x, make_example ex x)

make_example :: Example -> Int -> Example
make_example ex n = ex { 
    num_input = length (actions ex) + 1 - n,
    num_held_out = n 
}

-- example_1 is an attempt to show all the possible combinations of situations.
state_1 :: [String]
state_1 = [
    "m...",
    ".b..",
    "..b.",
    "...."
    ]

example_1 :: Example
example_1 = Example {
    initial_state = state_1,
    actions = [ActionRight, ActionDown, ActionDown, ActionUp, ActionRight, ActionRight, ActionDown, ActionDown, ActionLeft, ActionUp, ActionUp, ActionRight, ActionUp, ActionLeft, ActionLeft, ActionDown, ActionDown, ActionRight, ActionDown, ActionLeft, ActionRight],
    num_input = 0,
    num_held_out = 0
}

-- example_2 is an attempt to show all the possible combinations of situations.
state_2 :: [String]
state_2 = [
    "....",
    ".bm.",
    "..b.",
    "...."
    ]

example_2 :: Example
example_2 = Example {
    initial_state = state_2,
    actions = [ActionUp, ActionLeft, ActionLeft, ActionDown, ActionDown, ActionRight, ActionUp, ActionRight, ActionUp, ActionLeft, ActionDown, ActionRight, ActionDown, ActionLeft, ActionDown, ActionRight, ActionUp, ActionLeft, ActionDown, ActionRight, ActionUp],
    num_input = 0,
    num_held_out = 0
}

-- example_3 only shows one block, but the block is pushed in all directions.
state_3 :: [String]
state_3 = [
    "....",
    "....",
    ".b..",
    "m..."
    ]

example_3 :: Example
example_3 = Example {
    initial_state = state_3,
    actions = [ActionUp, ActionRight, ActionDown, ActionRight, ActionRight, ActionUp, ActionLeft, ActionDown, ActionLeft, ActionUp, ActionLeft, ActionUp, ActionRight, ActionUp, ActionRight, ActionDown, ActionRight, ActionDown, ActionLeft, ActionLeft, ActionDown],
    num_input = 0,
    num_held_out = 0
}

-- example_4 only shows one block, and the block is only pushed horizontally
state_4 :: [String]
state_4 = [
    "m...",
    "....",
    "..b.",
    "...."
    ]

example_4 :: Example
example_4 = Example {
    initial_state = state_4,
    actions = [ActionRight, ActionRight, ActionRight, ActionDown, ActionDown,
    ActionLeft, ActionDown, ActionLeft, ActionLeft, ActionUp, ActionRight, ActionUp, ActionUp, ActionLeft, ActionDown, ActionRight, ActionDown, ActionRight, ActionDown, ActionRight, ActionUp],
    num_input = 0,
    num_held_out = 0
}

-- example_5 only shows one block, and the block is only pushed vertically
state_5 :: [String]
state_5 = [
    "m...",
    "....",
    "..b.",
    "...."
    ]

example_5 :: Example
example_5 = Example {
    initial_state = state_5,
    actions = [ActionDown, ActionDown, ActionDown, ActionRight, ActionRight, ActionUp, ActionRight, ActionUp, ActionUp, ActionLeft, ActionDown, ActionDown, ActionLeft, ActionUp, ActionUp, ActionRight, ActionDown, ActionDown, ActionLeft, ActionDown, ActionRight],
    num_input = 0,
    num_held_out = 0
}

-- An example that attempts to show enough combinations
state_6 :: [String]
state_6 = [
    "m...",
    ".b..",
    "....",
    "...b"
    ]

example_6 :: Example
example_6 = Example {
    initial_state = state_6,
    actions = [ActionRight, ActionDown, ActionLeft, ActionDown, ActionRight, ActionDown, ActionRight, ActionUp, ActionRight, ActionUp, ActionLeft, ActionUp, ActionLeft, ActionDown, ActionRight, ActionDown, ActionLeft, ActionDown, ActionLeft, ActionUp, ActionUp],
    num_input = 0,
    num_held_out = 0
}

-- An example that attempts to show enough combinations
state_7 :: [String]
state_7 = [
    "m...",
    ".b..",
    ".b..",
    "...."
    ]

example_7 :: Example
example_7 = Example {
    initial_state = state_7,
    actions = [ActionDown, ActionRight, ActionUp, ActionRight, ActionDown, ActionRight, ActionDown, ActionDown, ActionLeft, ActionLeft, ActionUp, ActionRight, ActionUp, ActionLeft, ActionUp, ActionLeft, ActionDown, ActionDown, ActionRight, ActionRight, ActionLeft],
    num_input = 0,
    num_held_out = 0    
}

-- An example that attempts to show enough combinations
state_8 :: [String]
state_8 = [
    "....",
    "mb..",
    ".b..",
    "...."
    ]

example_8 :: Example
example_8 = Example {
    initial_state = state_8,
    actions = [ActionRight, ActionUp, ActionRight, ActionRight, ActionDown, ActionDown, ActionLeft, ActionLeft, ActionRight, ActionUp, ActionLeft, ActionLeft, ActionDown, ActionRight, ActionRight, ActionRight, ActionUp, ActionUp, ActionLeft, ActionLeft, ActionRight],
    num_input = 0,
    num_held_out = 0    
}

-- An example that attempts to show enough combinations
state_9 :: [String]
state_9 = [
    "....",
    ".b..",
    ".b..",
    "..m."
    ]

example_9 :: Example
example_9 = Example {
    initial_state = state_9,
    actions = [ActionUp, ActionLeft, ActionUp, ActionLeft, ActionDown, ActionUp, ActionUp, ActionRight, ActionDown, ActionRight, ActionRight, ActionUp, ActionLeft, ActionDown, ActionLeft, ActionLeft, ActionUp, ActionRight, ActionLeft, ActionRight, ActionRight],
    num_input = 0,
    num_held_out = 0    
}

state_10 :: [String]
state_10 = [
    "...b",
    "....",
    "..b.",
    "..m."
    ]

example_10 :: Example
example_10 = Example {
    initial_state = state_10,
    actions = [ActionUp, ActionRight, ActionUp, ActionLeft, ActionUp, ActionLeft, ActionRight, ActionDown, ActionUp, ActionLeft, ActionDown, ActionLeft, ActionDown, ActionRight, ActionDown, ActionUp, ActionLeft, ActionRight, ActionRight, ActionLeft, ActionLeft],
    num_input = 0,
    num_held_out = 0    
}



state_test :: [String]
state_test = [
    "....",
    "....",
    ".b..",
    "m..."
    ]

example_test :: Example
example_test = Example {
    initial_state = state_test,
    actions = [ActionRight, ActionUp, ActionRight, ActionUp],
    num_input = 0,
    num_held_out = 0
}
