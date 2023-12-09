module Main where
import System.Environment
import Data.Char
import Data.List
import Debug.Trace


main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ map computePrevValue (lines text)
      _ -> putStrLn "Wrong number of arguments"


computePrevValue::String->Int
computePrevValue input = computeDifferences $ parseInts input

computeDifferences::[Int]->Int
computeDifferences nbs@(first:_)
  | all (== 0) nbs = 0
  | otherwise = first - computeDifferences  (zipWith (-) (tail nbs) nbs)
  
parseInts::String->[Int]
parseInts str = map read (words str)
