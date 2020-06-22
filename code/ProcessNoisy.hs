module Main where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Random as Random
import qualified System.Process as Process

-- usage: code/process_noisy results/noisy
main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [d] -> processFiles d
        _ -> error "Usage"

processFiles :: String -> IO ()
processFiles d = do
    ls <- Directory.listDirectory d
    triples <- mapM (processFile d) ls
    Monad.forM_ (List.sort triples) print_triple

print_triple :: (String, String, String) -> IO ()
print_triple (f, e, a) = putStrLn $ "File: " ++ f ++ "\tEntropy: " ++ e ++ "\tStatus: " ++ a

processFile :: String -> String -> IO (String, String, String)
processFile d f = do
    l <- readFile (d ++ "/" ++ f)
    let ls = lines l
    let l1 = head $ lines_starting_with "Solving " ls
    let lse = lines_starting_with "Entropy of bnn : " ls
    let lsa = lines_starting_with "Status: " ls
    let zs = zip lse lsa
    let (l2, l3) = best_pair zs
    return (l1, l2, l3)

best_pair :: [(String, String)] -> (String, String)    
best_pair [] = ("undefined", "undefined")
best_pair ps = last $ List.sortBy (\a -> \b -> compare (fst a) (fst b)) ps

lines_starting_with :: String -> [String] -> [String]
lines_starting_with p ls = map (drop (length p)) ls' where
    ls' = filter (List.isPrefixOf p) ls
