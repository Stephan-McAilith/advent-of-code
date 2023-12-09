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
        print $ sum $ map computeNextValue (lines text)
      _ -> putStrLn "Wrong number of arguments"


computeNextValue::String->Int
computeNextValue input = computeDifferences $ parseInts input

computeDifferences::[Int]->Int
computeDifferences nbs
  | all (== 0) nbs = 0
  | otherwise = last nbs + computeDifferences  (zipWith (-) (tail nbs) nbs)
  
parseInts::String->[Int]
parseInts str = map read (words str)
