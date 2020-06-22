module HouseTypes where

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

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data State = State { 
    house :: [String],
    sensors :: [[Int]],
    pos :: Pos,
    window_size :: Pos
} deriving (Eq, Ord)

type Pos = (Int, Int)

type Trajectory = [Moment]  

type Moment = (State, Action)

data Action = ActionNoop | ActionLeft | ActionRight | ActionUp | ActionDown deriving (Eq, Ord)

data Example = Example {
    initial_house :: [String],
    initial_pos :: Pos,
    initial_window_size :: Pos,
    actions :: [Action]
} deriving (Eq, Ord, Show)

instance Show Action where
    show ActionNoop = "noop"
    show ActionLeft = "west"
    show ActionRight = "east"
    show ActionUp = "north"
    show ActionDown = "south"

