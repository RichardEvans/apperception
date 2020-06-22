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

import OcclusionData

const_time_limit :: Int
const_time_limit = 10

update_obj :: World -> ObjState -> ObjState
update_obj w obj | state obj >= max_state obj = move_obj w obj { state = 1 }
update_obj w obj | otherwise = obj { state = state obj + 1 }

move_obj :: World -> ObjState -> ObjState
move_obj w obj = case occ_dir obj of
    DirLeft -> obj { x = prev_x w (x obj) }
    DirRight -> obj { x = next_x w (x obj) }

prev_x :: World -> Int -> Int
prev_x w 0 = max_x w - 1
prev_x _ x = x - 1

next_x :: World -> Int -> Int
next_x w x = case x + 1 >= max_x w of
    False -> x + 1
    True -> 0

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

name :: ObjState -> String
name obj = show (index obj)

full_name :: ObjState -> String
full_name obj = "m" ++ name obj

update_line :: String -> ObjState -> String
update_line l obj = cs1 ++ name obj ++ cs2 where
    cs1 = take (x obj) l
    cs2 = drop (x obj + 1) l

trajectory :: World -> [World]
trajectory w = w : map update_world (trajectory w)

print_trajectory :: World -> Int -> IO ()
print_trajectory w n = Monad.forM_ (take n (trajectory w)) $ \x -> do
    putStrLn (unlines(display_world  x))
    putStrLn $ "Occluded: " ++ concat (List.intersperse ", " (map name (occluded_objs x)))
    putStrLn ""
    putStrLn "----"

occluded_objs :: World -> [ObjState]
occluded_objs w = filter f (occ_objects w) where
    f obj = any (g obj) (occ_objects w)
    g obj obj' = x obj == x obj' && y obj < y obj'

write_occlusion_task :: String -> World -> Int -> IO ()
write_occlusion_task file w t = 
    writeFile file (unlines (occlusion_task w t))

occlusion_task :: World -> Int -> [String]
occlusion_task w t = comments w t ++ trajectory_text w t ++ elements_text w t ++ exclusions_text ++ space_text w

comments :: World -> Int -> [String]
comments w t = h ++ concat (map f ps) ++ [header, ""] where
    ps = zip [1..] traj
    traj= take t (trajectory w)
    h = [header, "% Auto-generated from Occlusion.hs", "%"]
    f (i, x) = ("% Time " ++ show i ++ ":") : map g (display_world x) ++ ["%"]
    g l = "% " ++ l
    header = "%--------------------------------------------------"

trajectory_text :: World -> Int -> [String]
trajectory_text w t = concat (map world_atoms ps) where
    ps = zip [1..] traj
    traj= take t (trajectory w)

world_atoms :: (Int, World) -> [String]
world_atoms (t, w) = map (object_atom w t) (occ_objects w)

object_atom :: World -> Int -> ObjState -> String
object_atom w t obj = p ++ "(s2(c_in, obj_" ++ full_name obj ++ ", obj_c" ++ show (cell_num w obj) ++ "), " ++ show t ++ ")." where
    p = case obj `elem` occluded_objs w of
        True -> "hidden"
        False -> "senses"    

cell_num :: World -> ObjState -> Int
cell_num w obj = (y obj) * (max_x w) + x obj + 1

elements_text :: World -> Int -> [String]
elements_text w t = ["", "% Elements"] ++ objs ++ [time] where
    objs = map f [1 .. max_x w * max_y w] ++ map g (occ_objects w)
    f i = "is_object(obj_c" ++ show i ++ ")."
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
space_rights w = concat (map (space_right w) [0 .. max_y w - 1])

space_right :: World -> Int -> [String]
space_right w y = map f_right ps where
    cs = map g cis
    cs2 = drop 1 cs ++ [head cs]
    ps = zip cs cs2
    g i = "obj_c" ++ show i
    cis = take (max_x w) [y * max_x w + 1 ..]
    f_right (obj, obj') = "permanent(isa2(p_right, " ++ obj ++ ", " ++ obj' ++ "))."

space_belows :: World -> [String]
space_belows w = concat (map (space_below w) [0 .. max_x w - 1])

space_below :: World -> Int -> [String]
space_below w x = map f [0 .. max_y w - 2] where
    f y = "permanent(isa2(p_below, " ++ obj1 y ++ ", " ++ obj2 y ++ "))."
    obj1 y = "obj_c" ++ show ((y * max_x w) + x + 1)
    obj2 y = "obj_c" ++ show ((y * max_x w) + x + 4)

-------------------------------------- Main -----------------------------------

main :: IO ()
main = do
    args <- Environment.getArgs
    putStrLn $ "Generating occlusion task for " ++ concat (List.intersperse " " args)
    case args of
        [] -> gen_all
        ["all"] -> gen_all
        [w, t] -> do
            let f = "data_misc/input_occlusion_" ++ w ++ ".lp"
            putStrLn $ "Generating file " ++ f
            gen_occlusion f w (read t)
        _ -> error "Usage: occlusion all or occlusion <world> <num-time-steps>"

gen_occlusion :: String -> String -> Int -> IO ()        
gen_occlusion f w_s t = case lookup w_s occlusion_worlds of
    Just w -> write_occlusion_task f w t
    Nothing -> error "World not found"

gen_all :: IO ()
gen_all = do
    Monad.forM_ occlusion_worlds $ \(s, w) -> do
        let f = "data_misc/input_occlusion_" ++ s ++ ".lp"
        putStrLn $ "Generating file " ++ f
        write_occlusion_task f w const_time_limit
    write_single_experiment

gen_single_experiment :: [String]
gen_single_experiment = hs ++ xs ++ ts where
    hs = ["#!/bin/bash", "", "case $(expr $1 + 1) in"]
    ts = ["esac"]
    xs = concat (map f (zip [1..] occlusion_worlds))
    f (n, (f, w)) = ["\t" ++ show n ++ " )", "\t\techo \"Solving occlusion world " ++ f ++ "...\"", "\t\ttime ./solve occlusion " ++ f, "\t\t;;"]

write_single_experiment :: IO ()
write_single_experiment = do
    let f = "single_occlusion_experiment.sh"
    putStrLn $ "Generating file " ++ f
    writeFile f (unlines gen_single_experiment)
    let c = "chmod 777 " ++ f
    Process.callCommand c

