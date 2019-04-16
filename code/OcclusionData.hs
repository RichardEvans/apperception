module OcclusionData where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Environment as Environment
import qualified System.Process as Process

import Interpretation
import SolveTemplates

data Dir = DirLeft | DirRight deriving (Eq, Ord, Show)

data ObjState = OS {
    index :: Int,
    x :: Int,
    y :: Int,
    occ_dir :: Dir,
    state :: Int,
    max_state :: Int
} deriving (Eq, Ord, Show)

data World = W {
    occ_objects :: [ObjState],
    max_x :: Int,
    max_y :: Int
} deriving (Eq, Ord, Show)

obj :: Int -> Int -> Int -> Dir -> Int -> ObjState
obj i x y d s = OS {
    index = i,
    x = x,
    y = y,
    occ_dir = d,
    state = 1,
    max_state = s
}

-------------------------------------- Data --------------------------------

w1 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 1 1 DirLeft 2,
        obj 3 0 2 DirRight 1
        ],        
    max_x = 3,
    max_y = 3
}

w2 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 3 1 DirLeft 2,
        obj 3 0 2 DirRight 1
        ],        
    max_x = 4,
    max_y = 3
}

w3 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 4 1 DirLeft 2,
        obj 3 0 2 DirRight 1
        ],        
    max_x = 5,
    max_y = 3
}

w4 = W {
    occ_objects = [
        obj 1 0 0 DirLeft 3,
        obj 2 0 1 DirRight 2,
        obj 3 0 2 DirLeft 1
        ],        
    max_x = 5,
    max_y = 3
}

w5 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 0 1 DirRight 2,
        obj 3 0 2 DirRight 1
        ],        
    max_x = 5,
    max_y = 3
}

w6 = W {
    occ_objects = [
        obj 1 0 0 DirLeft 3,
        obj 2 0 1 DirLeft 2,
        obj 3 0 2 DirLeft 1
        ],        
    max_x = 5,
    max_y = 3
}

w7 = W {
    occ_objects = [
        obj 1 0 0 DirLeft 3,
        obj 2 0 1 DirRight 2,
        obj 3 0 2 DirLeft 1,
        obj 4 0 3 DirRight 1
        ],        
    max_x = 5,
    max_y = 4
}

w8 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 0 1 DirLeft 2,
        obj 3 0 2 DirRight 1,
        obj 4 0 3 DirLeft 1
        ],        
    max_x = 5,
    max_y = 4
}

w9 = W {
    occ_objects = [
        obj 1 0 0 DirRight 2,
        obj 2 0 1 DirLeft 2,
        obj 3 0 2 DirRight 1,
        obj 4 0 3 DirLeft 1
        ],        
    max_x = 5,
    max_y = 4
}

w10 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 1 1 DirLeft 2,
        obj 3 2 2 DirRight 1,
        obj 4 3 3 DirLeft 1
        ],        
    max_x = 6,
    max_y = 4
}

w11 = W {
    occ_objects = [
        obj 1 0 0 DirLeft 3,
        obj 2 1 1 DirLeft 2,
        obj 3 2 2 DirLeft 1,
        obj 4 3 3 DirLeft 1
        ],        
    max_x = 6,
    max_y = 4
}

w12 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 1 1 DirRight 2,
        obj 3 2 2 DirRight 1,
        obj 4 3 3 DirRight 1
        ],        
    max_x = 6,
    max_y = 4
}

w13 = W {
    occ_objects = [
        obj 1 0 0 DirLeft 3,
        obj 2 1 1 DirRight 2,
        obj 3 2 2 DirLeft 1,
        obj 4 3 3 DirRight 1
        ],        
    max_x = 6,
    max_y = 4
}

w14 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 5 1 DirLeft 2,
        obj 3 2 2 DirLeft 1,
        obj 4 4 3 DirLeft 1
        ],        
    max_x = 6,
    max_y = 4
}

w15 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 1 1 DirLeft 2,
        obj 3 2 2 DirRight 1,
        obj 4 3 3 DirLeft 1
        ],        
    max_x = 7,
    max_y = 4
}

w16 = W {
    occ_objects = [
        obj 1 0 0 DirLeft 3,
        obj 2 1 1 DirLeft 2,
        obj 3 2 2 DirLeft 1,
        obj 4 3 3 DirLeft 1
        ],        
    max_x = 7,
    max_y = 4
}

w17 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 1 1 DirRight 2,
        obj 3 2 2 DirRight 1,
        obj 4 3 3 DirRight 1
        ],        
    max_x = 7,
    max_y = 4
}

w18 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 3 1 DirRight 2,
        obj 3 2 2 DirLeft 1,
        obj 4 5 3 DirLeft 1
        ],        
    max_x = 7,
    max_y = 4
}

w19 = W {
    occ_objects = [
        obj 1 0 0 DirRight 1,
        obj 2 3 1 DirRight 3,
        obj 3 2 2 DirLeft 2,
        obj 4 5 3 DirRight 1
        ],        
    max_x = 7,
    max_y = 4
}

w20 = W {
    occ_objects = [
        obj 1 0 0 DirRight 3,
        obj 2 3 1 DirLeft 2,
        obj 3 2 2 DirRight 1,
        obj 4 5 3 DirLeft 2,
        obj 5 5 4 DirRight 1
        ],        
    max_x = 7,
    max_y = 5
}

occlusion_worlds :: [(String, World)]
occlusion_worlds = [
    ("w1", w1),
    ("w2", w2),
    ("w3", w3),
    ("w4", w4),
    ("w5", w5),
    ("w6", w6),
    ("w7", w7),
    ("w8", w8),
    ("w9", w9),
    ("w10", w10),
    ("w11", w11),
    ("w12", w12),
    ("w13", w13),
    ("w14", w14),
    ("w15", w15),
    ("w16", w16),
    ("w17", w17),
    ("w18", w18),
    ("w19", w19),
    ("w20", w20)
    ]

