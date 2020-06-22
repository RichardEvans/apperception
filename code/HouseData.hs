module HouseData where

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

import HouseTypes

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

house_examples :: [(String, Example)]
house_examples = zip hs es where
    hs = map f [1..]
    f i = "H" ++ show i
    es = [ Example { initial_house = h, initial_pos = (0, 0), initial_window_size = (2, 2), actions = a } | h <- houses, a <- as]
    houses = [house_2x2_1, house_2x2_2, house_2x2_3, house_2x2_4]
    as = [actions_2x2_1, actions_2x2_2, actions_2x2_3, actions_2x2_4]

house_2x2_1 :: [String]
house_2x2_1 = [
    ".XX.",
    "X..X",
    "X..X",
    ".XX."
    ]

house_2x2_2 :: [String]
house_2x2_2 = [
    "XXXX",
    "..X.",
    ".X..",
    "XXXX"
    ]

house_2x2_3 :: [String]
house_2x2_3 = [
    ".XX.",
    "X..X",
    "XXXX",
    "X..X"
    ]

house_2x2_4 :: [String]
house_2x2_4 = [
    "XXXX",
    "XXX.",
    "XX..",
    "X..."
    ]

actions_2x2_1 :: [Action]
actions_2x2_1 = [ActionRight, ActionRight, ActionDown, ActionLeft, ActionLeft, ActionDown, ActionRight, ActionRight]

actions_2x2_2 :: [Action]
actions_2x2_2 = [ActionRight, ActionRight, ActionDown, ActionLeft, ActionLeft, ActionDown, ActionRight, ActionRight, ActionLeft, ActionLeft, ActionUp, ActionRight, ActionRight, ActionUp, ActionLeft, ActionLeft]

actions_2x2_3 :: [Action]
actions_2x2_3 = [ActionRight, ActionRight, ActionDown, ActionLeft, ActionLeft, ActionDown, ActionRight, ActionRight, ActionUp, ActionUp, ActionLeft, ActionDown, ActionDown, ActionLeft, ActionUp, ActionUp]

actions_2x2_4 :: [Action]
actions_2x2_4 = [ActionDown, ActionDown, ActionRight, ActionUp, ActionUp, ActionRight, ActionDown, ActionDown, ActionUp, ActionUp, ActionLeft, ActionDown, ActionDown, ActionLeft, ActionUp, ActionUp]

-------------------------------------------------------------------------------
-- 3x3 (currently unused)
-------------------------------------------------------------------------------

house_3x3_1 :: [String]
house_3x3_1 = [
    "X...X",
    ".X.X.",
    "..X..",
    ".X.X.",
    "X...X"
    ]

house_3x3_2 :: [String]
house_3x3_2 = [
    ".XXX.",
    "X...X",
    "X.X.X",
    "X...X",
    ".XXX."
    ]

actions_3x3_1 :: [Action]    
actions_3x3_1 = [ActionRight, ActionDown, ActionDown, ActionLeft]

actions_3x3_2 :: [Action]
actions_3x3_2 = [ActionRight, ActionRight, ActionRight, ActionDown, ActionLeft, ActionLeft, ActionLeft, ActionDown, ActionRight, ActionRight, ActionRight, ActionDown, ActionLeft, ActionLeft, ActionLeft]

actions_3x3_3 :: [Action]
actions_3x3_3 = [ActionRight, ActionRight, ActionRight, ActionDown, ActionLeft, ActionLeft, ActionLeft, ActionDown, ActionRight, ActionRight, ActionRight, ActionDown, ActionLeft, ActionLeft, ActionLeft, ActionRight, ActionRight, ActionRight, ActionUp, ActionLeft, ActionLeft, ActionLeft, ActionUp, ActionRight, ActionRight, ActionRight, ActionUp, ActionLeft, ActionLeft, ActionLeft]

