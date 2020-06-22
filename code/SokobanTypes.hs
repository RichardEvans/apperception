module SokobanTypes where

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

data State = State {
    cells :: Cells,
    man :: Pos,
    blocks :: [Pos]
} deriving (Eq, Ord, Show)

data Cells = Cells {
    bounds :: Pos,
    walls :: [Pos]
} deriving (Eq, Ord, Show)

type Pos = (Int, Int)

type Trajectory = [Moment]  

type Moment = (State, Action)

data Action = ActionNoop | ActionLeft | ActionRight | ActionUp | ActionDown deriving (Eq, Ord)

data Example = Example {
    initial_state :: [String],
    actions :: [Action],
    num_input :: Int,
    num_held_out :: Int
} deriving (Eq, Ord, Show)

instance Show Action where
    show ActionNoop = "noop"
    show ActionLeft = "west"
    show ActionRight = "east"
    show ActionUp = "north"
    show ActionDown = "south"

state_to_strings :: State -> [String]
state_to_strings s = s4 where
    s1 = empty_strings (bounds (cells s))
    s2 = List.foldl' f2 s1 (walls (cells s))
    s3 = List.foldl' f3 s2 (blocks s)
    s4 = update_strings s3 (man s) 'm'
    f2 strings pos = update_strings strings pos 'w'
    f3 strings pos = update_strings strings pos 'b'

update_strings :: [String] -> Pos -> Char -> [String]
update_strings ss (x,y) c = ss' where
    ss' = take (y-1) ss ++ [s'] ++ drop y ss
    s = ss !! (y-1)
    s' = take (x-1) s ++ [c] ++ drop x s

empty_strings :: Pos -> [String]
empty_strings (bx, by) = List.replicate by (List.replicate bx '.')