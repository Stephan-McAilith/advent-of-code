module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra
import Debug.Trace
import Data.Maybe

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ map (foldl  (\ currentValue x -> ((currentValue + ord x) * 17) `mod` 256) 0)$ splitOn "," text
      _ -> putStrLn "Wrong number of arguments"