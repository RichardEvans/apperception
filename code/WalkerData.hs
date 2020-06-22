module WalkerData where

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

import Interpretation
import SolveTemplates

data Dir = DirLeft | DirRight | DirDown | DirUp | NoMove deriving (Eq, Ord, Show)

type Program = [(Dir, Int)]

data ObjState = OS {
    obj_type :: String,
    name :: String,
    x :: Int,
    y :: Int,
    program :: Program,
    index :: Int,
    counter :: Int
} deriving (Eq, Ord, Show)

data World = W {
    occ_objects :: [ObjState],
    max_x :: Int,
    max_y :: Int,
    total_time :: Int
} deriving (Eq, Ord, Show)

type Sprite = [[Int]]

data PixelArray = PixelArray {
    pixels :: Map.Map (Int, Int) Bool,
    size_x :: Int,
    size_y :: Int,
    sprite_tiles :: [((Int, Int), Int)]
}

type SpriteMap = Map.Map String [Sprite]

k_num_variants :: Int
k_num_variants = 1

k_sprite_size :: Int
k_sprite_size = 5

all_types :: World -> [String]
all_types w = setify (map obj_type (occ_objects w))

obj :: String -> String -> Int -> Int -> Program -> ObjState
obj t n x y p = OS {
    obj_type = t,
    name = n,
    x = x,
    y = y,
    program = p,
    index = 0,
    counter = 0
}

-------------------------------------- Data --------------------------------

w0 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirDown, 1)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 10
}

w1 = W {
    occ_objects = [
        obj "type_1" "1" 1 0 [(DirRight, 1), (DirDown, 1), (DirLeft, 1), (DirUp, 1)],
        obj "type_2" "2" 0 3 [(DirRight, 2), (DirLeft, 2)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 10
}

w2 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirRight, 1), (DirDown, 1)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 14
}

w3 = W {
    occ_objects = [
        obj "type_1" "1" 0 3 [(DirUp, 4), (DirRight, 1)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 15
}

w4 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirRight, 1), (DirDown, 1)],
        obj "type_2" "2" 0 3 [(DirUp, 4), (DirRight, 1)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 15
}

w5 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirRight, 2), (DirDown, 1), (DirLeft, 2), (DirDown, 1)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 14
}

w6 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirRight, 2), (DirDown, 3), (DirLeft, 2), (DirUp, 3)],
        obj "type_2" "2" 1 1 [(DirDown, 1), (DirUp, 1)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 15
}

w7 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirDown, 1)],
        obj "type_2" "2" 1 0 [(DirUp, 1)],
        obj "type_1" "3" 2 0 [(DirDown, 1)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 15
}

w8 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirRight, 2), (DirDown, 1)],
        obj "type_2" "2" 0 3 [(DirUp, 2), (DirRight, 1)],
        obj "type_1" "3" 1 1 [(DirDown, 1), (DirUp, 1)]
        ],        
    max_x = 3,
    max_y = 4,
    total_time = 15
}

w9 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirDown, 1), (DirRight, 1)],
        obj "type_2" "2" 0 1 [(DirRight, 1)],
        obj "type_1" "3" 2 3 [(DirUp, 2), (DirLeft, 2)]
        ],        
    max_x = 4,
    max_y = 4,
    total_time = 15
}

w10 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirRight, 1)],
        obj "type_2" "2" 3 0 [(DirLeft, 1)]
        ],        
    max_x = 4,
    max_y = 3,
    total_time = 15
}

w11 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirRight, 1)],
        obj "type_2" "2" 4 0 [(DirLeft, 1)]
        ],        
    max_x = 5,
    max_y = 3,
    total_time = 15
}

w12 = W {
    occ_objects = [
        obj "type_1" "1" 0 0 [(DirRight, 1)],
        obj "type_2" "2" 0 2 [(NoMove, 1)]
        ],        
    max_x = 3,
    max_y = 3,
    total_time = 15
}

walker_worlds :: [(String, World)]
walker_worlds = [
    ("w0", w0),
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
    ("w12", w12)
    ]

walker_sprite :: Sprite
walker_sprite = [
    [1, 0, 0, 0, 1],
    [0, 1, 0, 1, 0],    
    [0, 0, 1, 0, 0],        
    [0, 1, 0, 1, 0],    
    [1, 0, 0, 0, 1]
    ]

sitter_sprite :: Sprite
sitter_sprite = [
    [0, 1, 1, 1, 0],
    [1, 0, 0, 0, 1],    
    [1, 0, 0, 0, 1],    
    [1, 0, 0, 0, 1],    
    [0, 1, 1, 1, 0]
    ]

blank_sprite :: Sprite
blank_sprite = [
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
    ]

sprite_assocs :: [(String, Sprite)]
sprite_assocs = [
    ("type_1", walker_sprite),
    ("type_2", sitter_sprite),
    ("blank", blank_sprite)
    ]

variant_sprite_map :: Random.RandomGen r => Int -> r -> (r, SpriteMap)
variant_sprite_map n r = List.foldl' f (r, Map.empty) sprite_assocs where
    f (r2, m2) (t, s) = let (r3, vs) = n_sprite_variants n s r2 in (r3, Map.insert t vs m2)

n_sprite_variants :: Random.RandomGen r => Int -> Sprite -> r -> (r, [Sprite])
n_sprite_variants n s r = List.foldl' f (r, [s]) [1 .. n] where
    f (r2, l) _ = let (r3, s2) = sprite_variant (r2, s) in (r3, s2:l)

sprite_variant :: Random.RandomGen r => (r, Sprite) -> (r, Sprite)
sprite_variant (r, s) = (r', s') where
    (r', (x, y)) = pick r [(i, j) | i <- [0..4], j <- [0..4]]
    s' = flip_sprite s (x,y) 0

flip_sprite :: Sprite -> (Int, Int) -> Int -> Sprite
flip_sprite [] _ _ = []
flip_sprite (r:rs) (x,y) cy | y == cy = flip_row r x 0: flip_sprite rs (x,y) (cy+1)
flip_sprite (r:rs) (x,y) cy | otherwise = r : flip_sprite rs (x,y) (cy + 1)

flip_row :: [Int] -> Int -> Int -> [Int]
flip_row [] _ _ = []
flip_row (b:bs) x cx | x == cx = f b : bs where
    f 0 = 1
    f 1 = 0
flip_row (b:bs) x cx | otherwise = b : flip_row bs x (cx + 1)

pick :: Random.RandomGen r => r -> [a] -> (r, a)
pick _ [] = error "pickFromList wrongly given empty list"
pick r as =
    let indices = (0, length as-1)
        (i, r') = Random.randomR indices r
    in  (r', as !! i)

show_sprite :: Int -> Sprite -> String
show_sprite t s = unlines (map f s2) where
    s2 = zip [1 .. k_sprite_size * k_sprite_size] (concat s)
    f (i, x) = "bnn_input(ex_" ++ show t ++ ", node(1, " ++ show i ++ "), " ++ show x ++ ").  "
 
pretty_show_sprite :: Int -> Sprite -> String
pretty_show_sprite t s = unlines (tt : map f s) where
    tt = "% Tile " ++ show t
    f x = concat (map show x)


show_pixel_array_at_time :: (PixelArray, Int) -> String
show_pixel_array_at_time (p, t) = unlines [tt, pt] where
    tt = "% Time: " ++ show t
    pt = show_pixel_array p

show_pixel_array :: PixelArray -> String
show_pixel_array p = unlines (map f [0 .. size_y p - 1]) where
    f y = "% " ++ map (g y) [0 .. size_x p - 1]
    g y x = case Map.lookup (x,y) (pixels p) of
        Nothing -> error "pixel lookup fail"
        Just b -> case b of
            True -> '⬛'
            False -> '⬜'

render_trajectory :: Random.RandomGen r => r -> [World] -> (Map.Map Sprite Int, [PixelArray])
render_trajectory r ws = (sprite_index_map sm, pas) where
    (r2, sm) = variant_sprite_map k_num_variants r
    (_, pas) = List.foldl' f (r2, []) ws
    f (r3, pas2) w = let (r4, pa) = render_world r3 sm w in (r4, pas2 ++ [pa])

world_to_types :: World -> [((Int, Int), String)]
world_to_types w = concat $ map f [0 .. max_y w - 1] where
    f y = map (g y) [0 .. max_x w - 1]
    g y x = case find_obj x y w of
        Just obj -> ((x, y), obj_type obj)
        Nothing -> ((x, y), "blank")

find_obj :: Int -> Int -> World -> Maybe ObjState
find_obj i j w = List.find f (occ_objects w) where
    f obj = x obj == i && y obj == j

sprite_index_map :: SpriteMap -> Map.Map Sprite Int
sprite_index_map sm = Map.fromList (zip ss [0..]) where
    ss = concat (Map.elems sm)

render_world :: Random.RandomGen r => r -> SpriteMap -> World -> (r, PixelArray)
render_world r sm w = List.foldl' f (r, init_p) (world_to_types w) where
    init_p = blank_pixel_array w
    f (r2, p) ((i,j), t) = case Map.lookup t sm of
        Nothing -> error "Sprite type lookup fail"
        Just ss -> let (r3, s) = pick r2 ss in (r3, render_sprite sm p (i,j) s)

render_sprite :: SpriteMap -> PixelArray -> (Int, Int) -> Sprite -> PixelArray
render_sprite sm p (i, j) s = p { pixels = pixels', sprite_tiles = ((i, j), e) : sprite_tiles p } where
    pixels' = List.foldl' f (pixels p) (xyps (i, j) (concat s) (0, 0))
    f m ((x,y), 1) = Map.insert (x, y) True m
    f m ((x,y), 0) = Map.insert (x, y) False m
    Just e = Map.lookup s (sprite_index_map sm)

xyps :: (Int, Int) -> [Int] -> (Int, Int) -> [((Int, Int), Int)]
xyps _ [] _ = []
xyps (i, j) (b:bs) (sx, sy) = ((i*k_sprite_size+sx, j*k_sprite_size+sy), b) : xyps (i, j) bs (sx', sy') where
    (sx', sy') = case sx+1 >= k_sprite_size of
        True -> (0, sy+1)
        False -> (sx+1, sy)

blank_pixel_array :: World -> PixelArray
blank_pixel_array w = PixelArray { pixels = p, size_x = sx, size_y = sy, sprite_tiles = [] } where
    p = Map.fromList [((i, j), False) | i <- [0 .. sx - 1], j <- [0 .. sy - 1]]
    sx = max_x w * k_sprite_size
    sy = max_y w * k_sprite_size

draw_world :: World -> IO ()
draw_world w = do
    r <- Random.newStdGen
    let (r2, sm) = variant_sprite_map k_num_variants r
    let (_, p) = render_world r2 sm w
    putStrLn (show_pixel_array p)
    Monad.forM_ (reverse $ sprite_tiles p) $ \((i, j), e) -> do
        putStrLn $ "At (" ++ show i ++ ", " ++ show j ++ "): " ++ show e

