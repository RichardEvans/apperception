module SokobanPixels where

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
-- Types
-------------------------------------------------------------------------------

data PixelTrajectory = PT {
    sprite_index :: Map.Map Sprite Int,
    pixel_arrays :: [PixelArray],
    held_outs :: [PixelHeldOutEvaluationInfo]
} deriving (Eq, Ord, Show)

type Sprite = [[Int]]

type XCoord = Int
type YCoord = Int
type SpriteIndex = Int

data PixelArray = PixelArray {
    pixels :: Map.Map (Int, Int) Bool,
    size_x :: Int,
    size_y :: Int,
    sprite_tiles :: [((XCoord, YCoord), SpriteIndex)],
    action :: Action,
    p_state :: State
} deriving (Eq, Ord, Show)

type SpriteMap = Map.Map String [Sprite]

data PixelHeldOutEvaluationInfo = P { 
    time_step :: Int,
    possibles :: [PixelArray], 
    impossibles :: [PixelArray]
} deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Consts
-------------------------------------------------------------------------------

k_num_variants :: Int
k_num_variants = 3

k_sprite_size :: Int
k_sprite_size = 5

-------------------------------------------------------------------------------
-- Pixel trajectory generation
-------------------------------------------------------------------------------

pixel_trajectory :: Random.RandomGen r => r -> Trajectory -> Int -> PixelTrajectory
pixel_trajectory r ms num_held_out = pt where
    pt = PT { sprite_index = sprite_index_map sm, pixel_arrays = pas, held_outs = hos } 
    (r2, sm) = variant_sprite_map k_num_variants r
    (r3, pas) = List.foldl' f (r2, []) ms
    f (r', pas2) m = let (r4, pa) = render_moment r' sm m in (r4, pas2 ++ [pa])
    first_held_out = length pas + 1 - num_held_out
    (_, hos) = List.foldl' g (r3, []) [first_held_out .. first_held_out + num_held_out - 1] 
    g (r', acc) t = let (r'', i) = gen_held_out_evaluation_info r' ms sm t in (r'', acc++[i])

man_sprite :: Sprite
man_sprite = [
    [1, 0, 0, 0, 1],
    [0, 1, 0, 1, 0],    
    [0, 0, 1, 0, 0],        
    [0, 1, 0, 1, 0],    
    [1, 0, 0, 0, 1]
    ]

block_sprite :: Sprite
block_sprite = [
    [0, 1, 1, 1, 0],
    [1, 1, 1, 1, 1],    
    [1, 1, 1, 1, 1],    
    [1, 1, 1, 1, 1],    
    [0, 1, 1, 1, 0]
    ]

--wall_sprite :: Sprite
--wall_sprite = [
--    [0, 1, 0, 1, 0],
--    [1, 0, 1, 0, 1],    
--    [0, 1, 0, 1, 0],    
--    [1, 0, 1, 0, 1],    
--    [0, 1, 0, 1, 0]
--    ]

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
    ("man", man_sprite),
    ("block", block_sprite),
    -- ("wall", wall_sprite),
    ("blank", blank_sprite)
    ]

e9_sprite_map :: SpriteMap
e9_sprite_map = Map.fromList [
    ("man", [man_sprite, man_sprite_variation_1, man_sprite_variation_2, man_sprite_variation_3]),
    ("block", [block_sprite, block_sprite_variation_1, block_sprite_variation_2, block_sprite_variation_3]),
    ("blank", [blank_sprite, blank_sprite_variation_1, blank_sprite_variation_2, blank_sprite_variation_3])
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

sprite_index_map :: SpriteMap -> Map.Map Sprite Int
sprite_index_map sm = Map.fromList (zip ss [0..]) where
    ss = concat (Map.elems sm)

state_to_types :: State -> [((Int, Int), String)]
state_to_types s = List.sort (m : bs ++ ws ++ blanks) where
    m = (man s, "man")    
    bs = map fb (blocks s)
    ws = map fw (walls (cells s))
    fb p = (p, "block")
    fw p = (p, "wall")
    blanks = map fblank blank_ps
    (bx, by) = bounds (cells s)
    blank_ps = filter g [(i, j) | i <- [1..bx], j <- [1..by]]
    g pos = not (pos `elem` man s : blocks s ++ walls (cells s))
    fblank p = (p, "blank")

render_moment :: Random.RandomGen r => r -> SpriteMap -> Moment -> (r, PixelArray)
render_moment r sm (s,a) = List.foldl' f (r, init_p) (state_to_types s) where
    init_p = blank_pixel_array s a
    f (r2, p) ((i,j), t) = case Map.lookup t sm of
        Nothing -> error "Sprite type lookup fail"
        Just ss -> let (r3, s) = pick r2 ss in (r3, render_sprite sm p (i,j) s)

blank_pixel_array :: State -> Action -> PixelArray
blank_pixel_array s a = PixelArray { pixels = p, size_x = sx, size_y = sy, sprite_tiles = [], action = a, p_state = s } where
    p = Map.fromList [((i, j), False) | i <- [0 .. sx - 1], j <- [0 .. sy - 1]]
    (max_x, max_y) = bounds (cells s)
    sx = max_x * k_sprite_size
    sy = max_y * k_sprite_size

render_sprite :: SpriteMap -> PixelArray -> (Int, Int) -> Sprite -> PixelArray
render_sprite sm p (i, j) s = p { pixels = pixels', sprite_tiles = ((i, j), e) : sprite_tiles p } where
    pixels' = List.foldl' f (pixels p) (xyps (i-1, j-1) (concat s) (0, 0))
    f m ((x,y), 1) = Map.insert (x, y) True m
    f m ((x,y), 0) = Map.insert (x, y) False m
    Just e = Map.lookup s (sprite_index_map sm)

xyps :: (Int, Int) -> [Int] -> (Int, Int) -> [((Int, Int), Int)]
xyps _ [] _ = []
xyps (i, j) (b:bs) (sx, sy) = ((i*k_sprite_size+sx, j*k_sprite_size+sy), b) : xyps (i, j) bs (sx', sy') where
    (sx', sy') = case sx+1 >= k_sprite_size of
        True -> (0, sy+1)
        False -> (sx+1, sy)

-------------------------------------------------------------------------------
-- Pixel trajectory output
-------------------------------------------------------------------------------

trajectory_output :: PixelTrajectory -> [String]
trajectory_output pt = pixels_text ++ sprite_map_text ++ sprite_tiles_text ++ hos where
    sm = sprite_index pt
    last_time = length (pixel_arrays pt)
    num_inputs = last_time - length (held_outs pt)
    pas = take num_inputs (pixel_arrays pt)
    pixels_text = "% Pixels:" : "": map show_pixel_array_at_time (zip pas [1..])
    asm = List.sortBy (\a -> \b -> compare (snd a) (snd b)) (Map.assocs sm)
    sprite_map_text = "% Sprite map:" : "": (map (\(s, i) -> show_sprite i s) asm)
    sprite_tiles_text = "% Sprite tiles:" : "": map f (zip [1..] pas)
    f (t, p) = unlines $ map (g t) (reverse $ sprite_tiles p)
    g t ((i, j), e) = "sprite_at(" ++ show t ++ ", obj_cell_" ++ show i ++ "_" ++ show j ++ ", ex_" ++ show e ++ ")."
    held_out_times = [num_inputs + 1 .. last_time]
    hos = concat (map f2 (zip held_out_times (held_outs pt)))
    f2 (t, ho) = g2 t ho ++ h2 t ho
    g2 t ho = "" : ("% Held-out possibles at time " ++ show t) : concat (map (show_held_out_possible t) (possibles ho))
    h2 t ho = "" : ("% Held-out impossibles at time " ++ show t) : concat (map (show_held_out_impossible t) (zip [1..] (impossibles ho)))

show_held_out_possible :: Int -> PixelArray -> [String]
show_held_out_possible t pa = pixels_text ++ test_possibles_text where
    pixels_text = ["% Pixels:", "", show_pixel_array_at_time (pa, t)]
    test_possibles_text = "% Sprite tiles:" : "": map g (reverse $ sprite_tiles pa)
    g ((i, j), e) = "test_sprite_at(" ++ show t ++ ", obj_cell_" ++ show i ++ "_" ++ show j ++ ", ex_" ++ show e ++ ")."

show_held_out_impossible :: Int -> (Int, PixelArray) -> [String]
show_held_out_impossible t (n, pa) = pixels_text ++ test_impossibles_text where
    pixels_text = ["% Pixels:", "", show_pixel_array_at_time (pa, t)]
    test_impossibles_text = "% Sprite tiles:" : "": map g (reverse $ sprite_tiles pa)
    g ((i, j), e) = "test_impossible_combination(" ++ show t ++ ", " ++ show n ++ ", obj_cell_" ++ show i ++ "_" ++ show j ++ ", ex_" ++ show e ++ ")."

show_pixel_array_at_time :: (PixelArray, Int) -> String
show_pixel_array_at_time (p, t) = unlines [tt, pt] where
    tt = "% Time: " ++ show t
    pt = show_pixel_array p

show_pixel_array :: PixelArray -> String
show_pixel_array p = unlines ("" : map g ss ++ "" : map f [0 .. size_y p - 1]) where
    f y = "% " ++ map (pixel_char p y) [0 .. size_x p - 1]
    ss = state_to_strings (p_state p)
    g s = "% " ++ s

pixel_char :: PixelArray -> Int -> Int -> Char    
pixel_char p y x = case Map.lookup (x,y) (pixels p) of
    Nothing -> error "pixel lookup fail"
    Just b -> case b of
        True -> '⬛'
        False -> '⬜'

show_sprite :: Int -> Sprite -> String
show_sprite t s = unlines (map f s2) where
    s2 = zip [1 .. k_sprite_size * k_sprite_size] (concat s)
    f (i, x) = "bnn_input(ex_" ++ show t ++ ", node(1, " ++ show i ++ "), " ++ show x ++ ").  "
 

--- Sprite variations

man_sprite_variation_1 :: Sprite
man_sprite_variation_1 = [
    [1, 0, 0, 0, 1],
    [0, 1, 0, 1, 0],    
    [1, 0, 1, 0, 0],        
    [0, 1, 0, 1, 0],    
    [1, 0, 0, 0, 1]
    ]

man_sprite_variation_2 :: Sprite
man_sprite_variation_2 = [
    [1, 0, 0, 0, 1],
    [0, 0, 0, 1, 0],    
    [0, 0, 1, 0, 0],        
    [0, 1, 0, 1, 0],    
    [1, 0, 0, 0, 1]
    ]

man_sprite_variation_3 :: Sprite
man_sprite_variation_3 = [
    [1, 0, 0, 0, 1],
    [0, 1, 0, 1, 0],    
    [0, 0, 1, 0, 1],        
    [0, 1, 0, 1, 0],    
    [1, 0, 0, 0, 1]
    ]

block_sprite_variation_1 :: Sprite
block_sprite_variation_1 = [
    [1, 1, 1, 1, 0],
    [1, 1, 1, 1, 1],    
    [1, 1, 1, 1, 1],    
    [1, 1, 1, 1, 1],    
    [0, 1, 1, 1, 0]
    ]

block_sprite_variation_2 :: Sprite
block_sprite_variation_2 = [
    [0, 1, 1, 1, 0],
    [1, 1, 1, 1, 1],    
    [1, 1, 1, 0, 1],    
    [1, 1, 1, 1, 1],    
    [0, 1, 1, 1, 0]
    ]

block_sprite_variation_3 :: Sprite
block_sprite_variation_3 = [
    [0, 1, 1, 1, 0],
    [1, 1, 1, 1, 1],    
    [1, 1, 1, 1, 1],    
    [1, 1, 1, 1, 0],    
    [0, 1, 1, 1, 0]
    ]

blank_sprite_variation_1 :: Sprite
blank_sprite_variation_1 = [
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [1, 0, 0, 0, 0]
    ]

blank_sprite_variation_2 :: Sprite
blank_sprite_variation_2 = [
    [1, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
    ]

blank_sprite_variation_3 :: Sprite
blank_sprite_variation_3 = [
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
    ]


-------------------------------------------------------------------------------
-- Held-out data
-------------------------------------------------------------------------------

gen_held_out_evaluation_info :: Random.RandomGen r => r -> Trajectory -> SpriteMap -> Int -> (r, PixelHeldOutEvaluationInfo)
gen_held_out_evaluation_info r ms sm t = (r3, p) where
    (r2, ps) = possible_final_pixel_arrays r ms sm t
    (r3, ips) = impossible_final_pixel_arrays r2 ms sm t
    p = P { possibles = ps, impossibles = ips, time_step = t } 

k_num_variations :: Int
k_num_variations = 3

possible_final_pixel_arrays :: Random.RandomGen r => r -> Trajectory -> SpriteMap -> Int -> (r, [PixelArray])
possible_final_pixel_arrays r ms sm t = (r2, pas) where
    m = ms !! (t-1)
    (r2, pas) = List.foldl' f (r, []) [1 .. k_num_variations]
    f (r', xs) _ = (r'', x:xs) where
        (r'', x) = render_moment r' sm m

impossible_final_pixel_arrays :: Random.RandomGen r => r -> Trajectory -> SpriteMap -> Int -> (r, [PixelArray])
impossible_final_pixel_arrays r ms sm t = (r2, pas) where
    last_m = ms !! (t-1)
    initial_ms = take t ms
    distinct_ms = List.nub $ filter p initial_ms
    p (s, _) = s /= fst last_m
    (r2, pas) = List.foldl' f (r, []) distinct_ms
    f (r', xs) m = (r'', x:xs) where
        (r'', x) = render_moment r' sm m
