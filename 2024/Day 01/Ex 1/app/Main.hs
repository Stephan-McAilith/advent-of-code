module Main where
import System.Environment
import Data.List.Extra

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ processLines (lines text)
      _ -> putStrLn "Wrong number of arguments"

processLines::[String]->Int
processLines lines = computeDifference $ zip (sort leftList)  (sort rightList)
  where (leftList, rightList) = unzip $ map parseLine lines

computeDifference::[(Int, Int)]->Int
computeDifference = foldr ((+).(\(l,r)->abs $ l - r))  0

parseLine::String->(Int, Int)
parseLine line =  (read left, read right)
  where [left, right] = splitOn "   " line