module Main where

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
import SokobanPixels
import SokobanData

empty_state :: Pos -> State
empty_state b = State {
    cells = Cells { bounds = b, walls = [] },
    man = (0, 0),
    blocks = []
}

strings_to_state :: [String] -> State
strings_to_state as = List.foldl' f (empty_state b) as' where
    as' = zip [1..] as
    f b (y, a) = List.foldl' (update_state y) b (zip [1..] a)    
    b = get_bounds as

update_state :: Int -> State -> (Int, Char) -> State
update_state y s (x, '.') = s
update_state y s (x, 'w') = s { cells = cells' } where
    c = cells s
    cells' = c { walls = walls c ++ [(x,y)] }
update_state y s (x, 'b') = s { blocks = blocks s ++ [(x,y)] }
update_state y s (x, 'm') = s { man = (x, y) }

get_bounds :: [String] -> Pos
get_bounds ss = (x, y) where
    x = length (head ss)
    y = length ss

example_to_trajectory :: Example -> Trajectory
example_to_trajectory e = List.reverse ms where
    ms = List.foldl' update_trajectory b (actions e)
    b = [(strings_to_state (initial_state e), ActionNoop)]

update_trajectory :: Trajectory -> Action -> Trajectory   
update_trajectory ((s,_) : t) a = ((s',ActionNoop) : (s,a) : t) where
    s' = perform_action s a

trajectory_to_strings :: Trajectory -> [String]
trajectory_to_strings t = concat (map f t) where
    f (s, a) = state_to_strings s ++ ["", show a, ""]

show_example :: Example -> IO ()
show_example e = do 
    let t = example_to_trajectory e
    print_trajectory t
    putStrLn $ "Length: " ++ show (length t)

print_trajectory :: Trajectory -> IO ()
print_trajectory t = Monad.forM_ (trajectory_to_strings t) putStrLn

perform_action :: State -> Action -> State
perform_action s ActionNoop = s
perform_action s ActionLeft = move_man s (-1, 0)
perform_action s ActionRight = move_man s (1, 0)
perform_action s ActionUp = move_man s (0, -1)
perform_action s ActionDown = move_man s (0, 1)

move_man :: State -> (Int, Int) -> State
move_man s (dx, dy) = s { man = man', blocks = blocks' } where
    (mx, my) = man s
    mx' = mx + dx
    my' = my + dy
    man' = (mx', my')
    bs = blocks s
    blocks' = case List.elemIndex man' bs of
        Nothing -> bs
        Just i -> let (mx'', my'') = (mx'+dx, my'+dy) in
            case (mx'', my'') `elem` bs of
                True -> error "Invalid push: no empty space"
                False -> insert_at (mx'', my'') i bs

insert_at :: a -> Int -> [a] -> [a]
insert_at x i xs = take i xs ++ [x] ++ drop (i+1) xs



-------------------------------------------------------------------------------
-- Output
-------------------------------------------------------------------------------

print_example :: Example -> IO ()
print_example e = do
    let ss = example_to_symbolic_strings e
    Monad.forM_ ss putStrLn

example_to_symbolic_strings :: Example -> [String]
example_to_symbolic_strings e = let t = example_to_trajectory e in
    comments t ++ trajectory_atoms t ++ exogenous_atoms t ++ elements t ++ concepts ++ times t ++ exists_uniques ++ xors ++ cell_adjacency t ++ p_walls t

example_to_pixel_strings :: Random.RandomGen r => r -> Example -> [String]
example_to_pixel_strings r e = comments t ++ ss ++ exogenous_atoms t ++ elements t ++ concepts ++ times t ++ exists_uniques ++ xors ++ cell_adjacency t ++ p_walls t where
    t = example_to_trajectory e
    pt = pixel_trajectory r t (num_held_out e)
    ss = trajectory_output pt

p_walls :: Trajectory -> [String]
p_walls ((s,_):_) = "% Walls" : map f cs where
    f (x,y) = "permanent(isa(" ++ g (x,y) ++ ", obj_cell_" ++ show x ++ "_" ++ show y ++ "))."
    ws = walls (cells s)
    g p = if p `elem` ws then "p_is_wall" else "p_is_not_wall"
    cs = [(i, j) | i <- [1..bx], j <- [1..by]]
    (bx, by) = bounds (cells s)

cell_adjacency :: Trajectory -> [String]
cell_adjacency ((s,_):_) = "% Cell adjacency" : p_rights b ++ p_belows b ++ [""] where
    b = bounds (cells s)

p_rights :: (Int, Int) -> [String]
p_rights (bx, by) = concat (map f [1 .. by]) where
    f y = map (g y) [1 .. bx-1]
    g y x = "permanent(isa2(p_right, obj_cell_" ++ show x ++ "_" ++ show y ++ ", obj_cell_" ++ show (x+1) ++ "_" ++ show y ++ "))."

p_belows :: (Int, Int) -> [String]
p_belows (bx, by) = concat (map f [1 .. by-1]) where
    f y = map (g y) [1 .. bx]
    g y x = "permanent(isa2(p_below, obj_cell_" ++ show x ++ "_" ++ show y ++ ", obj_cell_" ++ show x ++ "_" ++ show (y+1) ++ "))."

times :: Trajectory -> [String]
times t = ["% Time", "is_time(1.." ++ show (length t) ++ ").", ""]

exists_uniques :: [String]
exists_uniques = exists_unique "c_in_1" "t_1" ++ exists_unique "c_in_2" "t_2"

exists_unique :: String -> String -> [String]
exists_unique p t = [
    "% ∃! clause for " ++ p ++ " : at most one",
    ":-",
    "\tholds(s2(" ++ p ++ ", X, Y), T),",
    "\tholds(s2(" ++ p ++ ", X, Y2), T),",
    "\tY != Y2.",
    "",
    "% ∃! clause for "++ p ++ " : at least one",
    ":-",
    "\tpermanent(isa(" ++ t ++ ", X)),",
    "\tis_time(T),",
    "\tnot aux_" ++ p ++ "(X, T).",
    "",
    "aux_" ++ p ++ "(X, T) :-",
    "\tholds(s2(" ++ p ++ ", X, _), T).",
    "",
    "% Incompossibility for " ++ p,
    "incompossible(s2(" ++ p ++ ", X, Y), s2(" ++ p ++ ", X, Y2)) :-",
    "\tpermanent(isa(" ++ t ++ ", X)),",
    "\tpermanent(isa(t_cell, Y)),",
    "\tpermanent(isa(t_cell, Y2)),",
    "\tY != Y2.",
    ""
    ]

xors :: [String]
xors = [
    "% Exclusions",
    "% Every action is either noop, north, south, east, or west",
    "% ∀X : man, noop(X) ⊕ north(X) ⊕ south(X) ⊕ east(X) ⊕ west(X)",
    "",
    "% At most one"
    ]
    ++ concat (map at_most_one all_action_pairs) ++ [""] ++ 
    [
    "% At least one",
    ":-",
    "\tpermanent(isa(t_1, X)),",
    "\tis_time(T),",
    "\tnot holds(s(c_noop, X), T),",
    "\tnot holds(s(c_north, X), T),",
    "\tnot holds(s(c_south, X), T),",
    "\tnot holds(s(c_east, X), T),",
    "\tnot holds(s(c_west, X), T).",
    "",
    "% Incompossibility"
    ]
    ++ concat (map incompossible all_action_pairs) ++ [""]

at_most_one :: (String, String) -> [String]
at_most_one (a1, a2) = [
    ":-",
    "\tholds(s(" ++ a1 ++ ", X), T),",
    "\tholds(s(" ++ a2 ++ ", X), T)."
    ]

incompossible :: (String, String) -> [String]
incompossible (a1, a2) = [
    "incompossible(s(" ++ a1 ++ ", X), s(" ++ a2 ++ ", X)) :-",
    "\tpermanent(isa(t_1, X)).",
    ""
    ]

all_action_pairs :: [(String, String)]    
all_action_pairs = [(a1, a2) | a1 <- actions, a2 <- actions, a1 < a2] where
    actions = ["c_noop", "c_north", "c_south", "c_east", "c_west"]

concepts :: [String]
concepts = "% Concepts" : map f ss ++ [""] where
    f s = "is_concept(" ++ s ++ ")."
    ss = ["in_1", "in_2", "noop", "north", "south", "east", "west"]

comments :: Trajectory -> [String]
comments t = header ++ concat (map f (t')) ++ [line, ""] where
    t' = zip [1..] t
    header = line : "% Generated by Sokoban.hs" : line : "% " : []
    f (i, (s, a)) = g i : "% " : map ("% " ++ ) (state_to_strings s ++ ["", show a, ""])
    g i = "% Time " ++ show i ++ ":"
    line = "%--------------------------------------------------"

exogenous_atoms :: Trajectory -> [String]
exogenous_atoms t = "% Exogenous actions" : map f (zip [1..] t) ++ [""] where
    f (i, (_, a)) = "exogenous(s(c_" ++ show a ++ ", obj_x1), " ++ show i ++ ")."

trajectory_atoms :: Trajectory -> [String]
trajectory_atoms t = h : concat (map f (zip [1..] t)) ++ [""] where
    h = "% The given sequence"
    f (i, (s, _)) = man_in i s : blocks_in i s

man_in :: Int -> State -> String
man_in t s = "senses(s2(c_in_1, obj_x1, obj_cell_" ++ show mx ++ "_" ++ show my ++ "), " ++ show t ++ ")." where
    (mx, my) = man s
    -- TODO: do hidden as well as senses...

blocks_in :: Int -> State -> [String]
blocks_in t s = map f (zip [1..] (blocks s)) where
    f (i, (bx, by)) = "senses(s2(c_in_2, obj_x" ++ show (i+1) ++ ", obj_cell_" ++ show bx ++ "_" ++ show by ++ "), " ++ show t ++ ")."
    -- TODO: do hidden as well as senses...

elements :: Trajectory -> [String]
elements ((s,_):_) = h : m : bs ++ cs ++ cs2 ++ [""] where
    h = "% Elements"
    m = "is_object(obj_x1)."
    bs = map f [1 .. length (blocks s)]
    cs = map g [(i, j) | i <- [1..bx], j <- [1..by]]
    (bx, by) = bounds (cells s)
    f b = "is_object(obj_x" ++ show (b+1) ++ ")."
    g (x, y) = "is_object(obj_cell_" ++ show x ++ "_" ++ show y ++ ")."
    cs2 = map g2 [(i, j) | i <- [1..bx], j <- [1..by]]
    g2 (x, y) = "is_cell(obj_cell_" ++ show x ++ "_" ++ show y ++ ")."

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [] -> gen_symbolic_all
        ["symbolic"] -> gen_symbolic_all
        ["pixels"] -> gen_pixels_all
        ["nn"] -> gen_nn
        [e] -> case lookup e sokoban_examples of
            Just x -> gen_symbolic_sokoban e x
            Nothing -> error $ "No example called " ++ e
        _ -> error "Usage: sokoban all or sokoban <example>"

gen_symbolic_sokoban :: String -> Example -> IO ()
gen_symbolic_sokoban s e = do
    putStrLn $ "Processing example " ++ s
    let f = "data/sokoban/predict_" ++ s ++ ".lp"
    let ss = example_to_symbolic_strings e
    writeFile f (unlines ss)

gen_pixels_sokoban :: String -> Example -> IO ()
gen_pixels_sokoban s e = do
    putStrLn $ "Processing example " ++ s
    let f = "data/sok-pixels/predict_" ++ s ++ ".lp"
    r <- Random.newStdGen
    let ss = example_to_pixel_strings r e
    writeFile f (unlines ss)

gen_symbolic_all :: IO ()
gen_symbolic_all = do
    Monad.forM_ sokoban_examples $ \(s, e) -> gen_symbolic_sokoban s e
    write_symbolic_single_experiment

gen_pixels_all :: IO ()
gen_pixels_all = do
    Monad.forM_ sokoban_examples $ \(s, e) -> gen_pixels_sokoban s e
    write_pixels_single_experiment

gen_symbolic_single_experiment :: [String]
gen_symbolic_single_experiment = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f (zip [1..] sokoban_examples))
    f (n, (f, _)) = ["\t" ++ show n ++ " )", "\t\techo \"Solving sokoban example " ++ f ++ "...\"", "\t\ttime code/solve sokoban " ++ f, "\t\t;;"]

write_symbolic_single_experiment :: IO ()
write_symbolic_single_experiment = do
    let f = "scripts/single_sokoban.sh"
    putStrLn $ "Generating file " ++ f
    writeFile f (unlines gen_symbolic_single_experiment)
    let c = "chmod 777 " ++ f
    Process.callCommand c

gen_pixels_single_experiment :: [String]
gen_pixels_single_experiment = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f (zip [1..] sokoban_examples))
    f (n, (f, _)) = ["\t" ++ show n ++ " )", "\t\techo \"Solving sok-pixels example " ++ f ++ "...\"", "\t\ttime code/solve sok-pixels " ++ f, "\t\t;;"]

write_pixels_single_experiment :: IO ()
write_pixels_single_experiment = do
    let f = "scripts/single_sok_pixels.sh"
    putStrLn $ "Generating file " ++ f
    writeFile f (unlines gen_pixels_single_experiment)
    let c = "chmod 777 " ++ f
    Process.callCommand c

-------------------------------------------------------------------------------
-- Pixel Rendering
-------------------------------------------------------------------------------

test_pixel_trajectory :: Example -> IO ()
test_pixel_trajectory e = do
    r <- Random.newStdGen
    let t = example_to_trajectory e
    let pt = pixel_trajectory r t 3
    let ss = trajectory_output pt
    Monad.forM_ ss putStrLn

-------------------------------------------------------------------------------
-- Display variations
-------------------------------------------------------------------------------

show_variations :: IO ()
show_variations = do
    let t = example_to_trajectory example_1
    let [m] = t
    let f = "PixelSequenceE1.hs"
    let ls = ["module PixelSequenceE1 where", 
            "pixel_sequence_e9 :: [[String]]",
            "pixel_sequence_e9 = ["
            ]
    writeFile f (unlines ls)
    Monad.forM_ [1..256] $ \_ -> do
        r <- Random.newStdGen
        let (_, pa) = render_moment r e9_sprite_map m
        appendFile f (print_pixel_array pa)

print_pixel_array :: PixelArray -> String
print_pixel_array p = unlines ("    [" : map f [0 .. last_y] ++ ["    ],"]) where
    f y = "    \"" ++ map (pixel_c p y) [0 .. size_x p - 1] ++ (if y < last_y then "\"," else "\"")
    last_y = size_y p - 1

pixel_c :: PixelArray -> Int -> Int -> Char    
pixel_c p y x = case Map.lookup (x,y) (pixels p) of
    Nothing -> error "pixel lookup fail"
    Just b -> case b of
        True -> 'X'
        False -> 'O'

-------------------------------------------------------------------------------
-- trajectory generation
-------------------------------------------------------------------------------

all_legal_moves :: State -> [Action]
all_legal_moves s = filter (is_legal s) all_actions

all_actions :: [Action]
all_actions = [ActionNoop, ActionLeft, ActionRight, ActionUp, ActionDown]

is_legal :: State -> Action -> Bool
is_legal _ ActionNoop = True
is_legal s ActionLeft = legal_move s (-1, 0)
is_legal s ActionRight = legal_move s (1, 0)
is_legal s ActionUp = legal_move s (0, -1)
is_legal s ActionDown = legal_move s (0, 1)

legal_move :: State -> (Int, Int) -> Bool
legal_move s (dx, dy) = let (man_x, man_y) = man s in case is_empty_cell s (man_x + dx, man_y + dy) of
    True -> True
    False -> is_empty_cell s (man_x + dx + dx, man_y + dy + dy) 

is_empty_cell :: State -> (Int, Int) -> Bool
is_empty_cell s (x, y) | out_of_bounds s (x, y) = False
is_empty_cell s (x, y) | (x, y) `elem` walls (cells s) = False
is_empty_cell s (x, y) | (x, y) `elem` blocks s = False
is_empty_cell _ _ = True

out_of_bounds :: State -> (Int, Int) -> Bool
out_of_bounds s (x, y) = x <= 0 || x > fst (bounds (cells s)) || y <= 0 || y > snd (bounds (cells s))

random_state :: Random.RandomGen r => r -> Int -> Int -> (r, State)
random_state r size num_blocks = (r3, s) where
    s = State { cells = cs, man = m, blocks = bs }
    cs = Cells { bounds = (size, size), walls = [] }
    (r2, bs) = List.foldl' f (r, []) [1 .. num_blocks]
    f (r, bs) _ = (r', b:bs) where (r', b) = pick_pos r size bs
    (r3, m) = pick_pos r2 size bs

pick_pos :: Random.RandomGen r => r -> Int -> [Pos] -> (r, Pos)
pick_pos r size bs = case p `elem` bs of
    False -> (r', p)
    True -> pick_pos r' size bs
    where
        (r', p) = pick r ps
        ps = [(i, j) | i <- [1..size], j <- [1..size]]

random_action :: Random.RandomGen r => r -> State -> (r, Action)
random_action r s = pick r (all_legal_moves s)

random_trajectory :: Random.RandomGen r => r -> Int -> Int -> Int -> Trajectory
random_trajectory r size num_blocks len = ms where
    (r2, s) = random_state r size num_blocks
    (r3, a) = random_action r2 s
    (_, ms) = List.foldl' f (r3, [(s, a)]) [1 .. len-1]
    f (r, ms) _ = (r', ms ++ [m]) where (r', m) = random_next_moment r (last ms) 

random_next_moment :: Random.RandomGen r => r -> Moment -> (r, Moment)
random_next_moment r (s, a) = (r', (s', a')) where
    s' = perform_action s a
    (r', a') = random_action r s'

test_random_trajectory :: Int -> Int -> Int -> IO ()
test_random_trajectory size num_blocks len = do
    r <- Random.newStdGen
    let t = random_trajectory r size num_blocks len
    let s = trajectory_to_strings t
    Monad.forM_ s putStrLn

gen_nn :: IO ()
gen_nn = Monad.forM_ [1 .. 50000] $ \i -> random_pixel_trajectory i 4 2 20

random_pixel_trajectory :: Int -> Int -> Int -> Int -> IO ()   
random_pixel_trajectory i size num_blocks len = do
    r <- Random.newStdGen
    let t = random_trajectory r size num_blocks len
    r2 <- Random.newStdGen
    let pt = pixel_trajectory r2 t 1
    output_pixel_trajectory_files i pt

output_pixel_trajectory_files :: Int -> PixelTrajectory -> IO ()
output_pixel_trajectory_files i pt = do
    let f2 = "data/sok-nn/sok_" ++ show i ++ "_nn.lp"    
    let ss2 = nn_trajectory_output pt
    writeFile f2 (unlines ss2)
    putStrLn $ "Created " ++ f2

nn_trajectory_output :: PixelTrajectory -> [String]
nn_trajectory_output pt = concat (map g zs) where
    pas = pixel_arrays pt
    pixels_text = map show_pixel_array_at_time (zip pas [1..])
    actions_text = map f pas
    zs = zip pixels_text actions_text
    f pa = "Action: " ++ show (action pa)
    g (pt, at) = [pt, at, "", "~~~~~~", ""]

test_held_outs :: IO ()
test_held_outs = do
    let (_, e) = last sokoban_examples
    let t = example_to_trajectory e
    r <- Random.newStdGen
    let pt = pixel_trajectory r t 1
    let h = held_outs pt !! 0
    putStrLn "\nPossibles:"
    Monad.forM_ (possibles h) (putStrLn . show_pixel_array)
    putStrLn "\nImpossibles:"
    Monad.forM_ (impossibles h) (putStrLn . show_pixel_array)
    putStrLn "\nCode:"
    Monad.forM_ (trajectory_output pt) putStrLn
