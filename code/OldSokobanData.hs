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
sokoban_examples = [
    ("e0", example_0),
    ("e1", example_1),
    ("e2", example_2),
    ("e3", example_3),
    ("e4", example_4),
    ("e5", example_5),
    ("e6", example_6),
    ("e7", example_7),
    ("e8", example_8)
    ]

state_0 :: [String]
state_0 = [
    ".m..b..."
    ]

example_0 :: Example
example_0 = Example {
    initial_state = state_0,
    actions = [ActionRight, ActionNoop, ActionRight, ActionRight, ActionNoop, ActionRight, ActionRight]
}

state_1 :: [String]
state_1 = [
    "....b",
    "..b.b",
    ".....",
    "m...."
    ]

example_1 :: Example
example_1 = Example {
    initial_state = state_1,
    actions = [ActionUp, ActionNoop, ActionRight, ActionUp, ActionUp, ActionRight, ActionDown, ActionRight, ActionDown, ActionDown]
}

state_2 :: [String]
state_2 = [
    "....",
    "..b.",
    "mb.."
    ]

example_2 :: Example
example_2 = Example {
    initial_state = state_2,
    actions = [ActionRight, ActionRight, ActionUp, ActionRight, ActionUp]
}

state_3 :: [String]
state_3 = [
    ".",
    "b",
    "m",
    ".",
    "b",
    "."
    ]

example_3 :: Example
example_3 = Example {
    initial_state = state_3,
    actions = [ActionUp, ActionDown, ActionDown, ActionDown, ActionUp, ActionDown]
}

state_4 :: [String]
state_4 = [
    "b....",
    "..m..",
    "....."
    ]

example_4 :: Example
example_4 = Example {
    initial_state = state_4,
    actions = [ActionUp, ActionDown, ActionLeft, ActionRight, ActionRight, ActionLeft, ActionDown, ActionUp, ActionRight, ActionUp]
}

state_5 :: [String]
state_5 = [
    "....",
    "....",
    "..b.",
    "..m."
    ]

example_5 :: Example
example_5 = Example {
    initial_state = state_5,
    actions = [ActionUp, ActionRight, ActionUp, ActionUp, ActionLeft, ActionDown, ActionDown]
}

state_6 :: [String]
state_6 = [
    "....",
    "....",
    "..b.",
    "..m."
    ]

example_6 :: Example
example_6 = Example {
    initial_state = state_6,
    actions = [ActionUp, ActionRight, ActionUp, ActionUp, ActionLeft, ActionDown, ActionRight, ActionDown, ActionLeft, ActionDown, ActionLeft, ActionLeft, ActionUp, ActionRight, ActionRight]
}

state_7 :: [String]
state_7 = [
    "....",
    "....",
    "..b.",
    "..m."
    ]

example_7 :: Example
example_7 = Example {
    initial_state = state_7,
    actions = [ActionUp, ActionRight, ActionUp, ActionLeft, ActionUp, ActionLeft, ActionRight, ActionDown, ActionUp, ActionLeft, ActionDown, ActionLeft, ActionDown, ActionRight, ActionDown, ActionUp, ActionLeft, ActionRight, ActionLeft]
}

state_8 :: [String]
state_8 = [
    "...b",
    "....",
    "..b.",
    "..m."
    ]

example_8 :: Example
example_8 = Example {
    initial_state = state_8,
    actions = [ActionUp, ActionRight, ActionUp, ActionLeft, ActionUp, ActionLeft, ActionRight, ActionDown, ActionUp, ActionLeft, ActionDown, ActionLeft, ActionDown, ActionRight, ActionDown, ActionUp, ActionLeft]
}

state_9 :: [String]
state_9 = [
    "...b",
    "bm..",
    "....",
    "...."
    ]

example_9 :: Example
example_9 = Example {
    initial_state = state_9,
    actions = []
}

state_10 :: [String]
state_10 = [
    "....",
    ".mbb",
    ".b..",
    "...."
    ]

