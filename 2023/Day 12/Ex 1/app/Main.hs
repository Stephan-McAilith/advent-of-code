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
        print $ sum $ map processLine $ lines text
      _ -> putStrLn "Wrong number of arguments"

processLine line = countPossibilities springs defectNums
 where
  springs = head splitLine
  defectNums = map read $ splitOn "," $ last splitLine
  splitLine = splitOn " " line

countPossibilities [] (_:_) = 0
countPossibilities springs [] = if '#' `elem` springs then 0 else 1
countPossibilities ('.':springs) nums = countPossibilities springs nums
countPossibilities ('?':springs) nums = countPossibilities springs nums + countPossibilities ('#':springs) nums
countPossibilities springs@('#':_) (num:nums) = if length springs >= num && all (`elem` "#?") (take num springs) && (length springs == num || (springs !! num) `elem` ".?") then countPossibilities (drop (num + 1) springs) nums else 0