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

import RandomPairs
import WalkerData

flag_generate_looks :: Bool
flag_generate_looks = True

const_time_limit :: Int
const_time_limit = 10

update_obj :: World -> ObjState -> ObjState
update_obj w obj = update_state (move_obj w obj)

move_obj :: World -> ObjState -> ObjState
move_obj w obj = case current_dir obj of
    DirLeft -> obj { x = prev_x w (x obj) }
    DirRight -> obj { x = next_x w (x obj) }
    DirUp -> obj { y = prev_y w (y obj) }
    DirDown -> obj { y = next_y w (y obj) }
    NoMove -> obj

current_dir :: ObjState -> Dir
current_dir obj = fst (program obj !! index obj)

prev_x :: World -> Int -> Int
prev_x w 0 = max_x w - 1
prev_x _ x = x - 1

next_x :: World -> Int -> Int
next_x w x = case x + 1 >= max_x w of
    False -> x + 1
    True -> 0

prev_y :: World -> Int -> Int
prev_y w 0 = max_y w - 1
prev_y _ y = y - 1

next_y :: World -> Int -> Int
next_y w y = case y + 1 >= max_y w of
    False -> y + 1
    True -> 0

update_state :: ObjState -> ObjState    
update_state obj = let counter' = counter obj + 1 in
    case counter' >= snd (program obj !! index obj) of
        True -> obj { index = next_index obj, counter = 0 }
        False -> obj { counter = counter' }

next_index :: ObjState -> Int        
next_index obj = let index' = index obj + 1 in
    case index' >= length (program obj) of
        True -> 0
        False -> index'

update_world :: World -> World
update_world w = w { occ_objects = objs' } where
    objs' = map (update_obj w) (occ_objects w)

display_world :: World -> [String]
display_world w = List.foldl' update_lines (empty_world w) (occ_objects w)

empty_world :: World -> [String]
empty_world w = replicate (max_y w) (replicate (max_x w) '.')

update_lines :: [String] -> ObjState -> [String]
update_lines w obj = ls1 ++ [l] ++ ls2 where
    ls1 = take (y obj) w
    l = update_line (w !! y obj) obj
    ls2 = drop (y obj + 1) w

full_name :: ObjState -> String
full_name obj = "w" ++ name obj

update_line :: String -> ObjState -> String
update_line l obj = cs1 ++ name obj ++ cs2 where
    cs1 = take (x obj) l
    cs2 = drop (x obj + 1) l

trajectory :: World -> [World]
trajectory w = w : map update_world (trajectory w)

print_trajectory :: World -> Int -> IO ()
print_trajectory w n = Monad.forM_ (take n (trajectory w)) $ \x -> do
    putStrLn (unlines(display_world  x))
    putStrLn ""
    putStrLn "----"

draw_trajectory :: World -> Int -> IO ()
draw_trajectory w n = do
    let t = take n (trajectory w)
    r <- Random.newStdGen
    let (sm, pas) = render_trajectory r t
    let o = trajectory_output (sm, pas)
    putStrLn (unlines o)

trajectory_output :: (Map.Map Sprite Int, [PixelArray]) -> [String]
trajectory_output (sm, pas) = pixels_text ++ sprite_map_text ++ sprite_tiles_text where
    pixels_text = "% Pixels:" : "": map show_pixel_array_at_time (zip pas [1..])
    asm = List.sortBy (\a -> \b -> compare (snd a) (snd b)) (Map.assocs sm)
    sprite_map_text = "% Sprite map:" : "": (map (\(s, i) -> show_sprite i s) asm)
    sprite_tiles_text = "% Sprite tiles:" : "": map f (zip [1..] pas)
    f (t, p) = unlines $ map (g t) (reverse $ sprite_tiles p)
    g t ((i, j), e) = h t ++ "sprite_at(" ++ show t ++ ", obj_c_" ++ show i ++ "_" ++ show j ++ ", ex_" ++ show e ++ ")."
    h t | t == length pas = "test_"
    h t | otherwise = ""

write_walker_task :: TaskType -> String -> World -> Int -> IO ()
write_walker_task task file w t = do
    ss <- walker_task task w t
    writeFile file (unlines ss)

walker_task :: TaskType -> World -> Int -> IO [String]
walker_task task w t = do
    tt <- trajectory_text task w t
    let r = comments w t ++ tt ++ elements_text w t ++ exclusions_text ++ space_text w
    return r

comments :: World -> Int -> [String]
comments w t = h ++ concat (map f ps) ++ [header, ""] where
    ps = zip [1..] traj
    traj= take t (trajectory w)
    h = [header, "% Auto-generated from Walker.hs", "%"]
    f (i, x) = ("% Time " ++ show i ++ ":") : map g (display_world x) ++ ["%"]
    g l = "% " ++ l
    header = "%--------------------------------------------------"

trajectory_text :: TaskType -> World -> Int -> IO [String]
trajectory_text task w t = do
    let traj= take t (trajectory w)
    r <- Random.newStdGen
    let (sm, pas) = render_trajectory r traj
    let o = trajectory_output (sm, pas)
    return o    

world_atoms :: [(Int, Int)] -> Int -> (Int, World) -> [String]
world_atoms hps max_t (t, w) = map (object_atom hps max_t w t) (zip [1..] (occ_objects w))

object_atom :: [(Int, Int)] -> Int -> World -> Int -> (Int, ObjState) -> String
object_atom hps max_t w t (k, obj) | flag_generate_looks == False = p ++ "(s2(c_in, obj_" ++ full_name obj ++ ", " ++ cell_string obj ++ "), " ++ show t ++ ")." where
    p = case (t, k) `elem` hps of
        True -> "hidden"
        False -> "senses"    
object_atom hps max_t w t (k, obj) | flag_generate_looks = "looks_" ++ obj_type obj ++ "(" ++ cell_string obj ++ ", " ++ show t ++ ")."

cell_string :: ObjState -> String
cell_string obj = "obj_c_" ++ show (x obj) ++ "_" ++ show (y obj)

elements_text :: World -> Int -> [String]
elements_text w t = ["", "% Elements"] ++ objs ++ [time] where
    objs = cells ++ cells2 ++ map g (occ_objects w)
    cells = ["is_object(obj_c_" ++ show i ++ "_" ++ show j ++ ")." | i <- [0 .. max_x w-1], j <- [0 .. max_y w - 1]]
    cells2 = ["is_cell(obj_c_" ++ show i ++ "_" ++ show j ++ ")." | i <- [0 .. max_x w-1], j <- [0 .. max_y w - 1]]
    g obj = "is_object(obj_" ++ full_name obj ++ ")."
    time = "is_time(1.." ++ show t ++ ")."

exclusions_text :: [String]
exclusions_text = [
    "",
    "is_concept(c_in).",
    "",
    "% ∃! clause for c_in : at most one",
    ":-",
    "   holds(s2(c_in, X, Y), T),",
    "   holds(s2(c_in, X, Y2), T),",
    "   Y != Y2.",
    "",
    "% ∃! clause for c_in : at least one",
    ":-",
    "   permanent(isa(t_mover, X)),",
    "   is_time(T),",
    "   not aux_c_in(X, T).",
    "",
    "aux_c_in(X, T) :-",
    "   holds(s2(c_in, X, _), T).",
    "",
    "% Incompossibility for in",
    "incompossible(s2(c_in, X, Y), s2(c_in, X, Y2)) :-",
    "   permanent(isa(t_mover, X)),",
    "   permanent(isa(t_cell, Y)),",
    "   permanent(isa(t_cell, Y2)),",
    "   Y != Y2."
    ]

space_text :: World -> [String]
space_text w = "" : space_rights w ++ space_belows w

space_rights :: World -> [String]
space_rights w = concat (map (space_right w) [1 .. max_y w])

space_right :: World -> Int -> [String]
space_right w y = extra : map f [1 .. max_x w - 1] where
    f x = "permanent(isa2(p_right, " ++ g x ++ ", " ++ g (x+1) ++ "))." 
    g x = "obj_c_" ++ show (x-1) ++ "_" ++ show (y-1)
    extra = "permanent(isa2(p_right, " ++ g (max_x w) ++ ", " ++ g 1 ++ "))."

space_belows :: World -> [String]
space_belows w = concat (map (space_below w) [1 .. max_x w])

space_below :: World -> Int -> [String]
space_below w x = extra : map f [1 .. max_y w - 1] where
    f y = "permanent(isa2(p_below, " ++ g y ++ ", " ++ g (y+1) ++ "))."
    g y = "obj_c_" ++ show (x-1) ++ "_" ++ show (y-1)
    extra = "permanent(isa2(p_below, " ++ g (max_y w) ++ ", " ++ g 1 ++ "))."

-------------------------------------- Main -----------------------------------

main :: IO ()
main = do
    args <- Environment.getArgs
    putStrLn $ "Generating walker task for " ++ concat (List.intersperse " " args)
    case args of
        [] -> gen_all
        ["all"] -> gen_all
        [w, t] -> do
            let f = "data/walker/predict_" ++ w ++ ".lp"
            putStrLn $ "Generating file " ++ f
            gen_walker TaskPrediction f w (read t)
        _ -> error "Usage: walker all or walker <world> <num-time-steps>"

gen_walker :: TaskType -> String -> String -> Int -> IO ()        
gen_walker task f w_s t = case lookup w_s walker_worlds of
    Just w -> write_walker_task task f w t
    Nothing -> error "World not found"

gen_all :: IO ()
gen_all = do
    -- Currently only prediction is supported...
    -- let ts = [TaskPrediction, TaskRetrodiction, TaskImputation]
    let ts = [TaskPrediction]
    Monad.forM_ ts $ \task ->
        Monad.forM_ walker_worlds $ \(s, w) -> do
            let f = "data/walker/" ++ show task ++ "_" ++ s ++ ".lp"
            putStrLn $ "Generating file " ++ f
            write_walker_task task f w (total_time w)

