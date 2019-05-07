module Main where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Text.Printf as Printf
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Process as Process

-------------------------------------- Types ----------------------------------

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [d] -> gen_single d
        _ -> error "Usage: get_single <data folder>"

gen_single :: String -> IO ()
gen_single d = do
    fs <- Directory.listDirectory ("data/" ++ d)
    let fs' = List.sort $ filter is_input fs
    output_single d fs'

is_input :: String -> Bool
is_input f = drop (length f - 3) f == ".lp"

output_single :: String -> [String] -> IO ()
output_single d fs = do
    let f = "scripts/single_" ++ d ++ ".sh"
    writeFile f "#!/bin/bash\n\n"
    appendFile f "case $(expr $1 + 1) in\n"
    Monad.forM_ (zip [1..] fs) $ \(i,x) -> do
        appendFile f $ "\t" ++ (show i) ++ " )\n"
        appendFile f $ "\t\ttime code/solve " ++ d ++ " " ++ x ++ "\n"
        appendFile f $ "\t\t;;\n"
    appendFile f "esac\n"
    let c = "chmod 777 " ++ f
    Process.callCommand c

