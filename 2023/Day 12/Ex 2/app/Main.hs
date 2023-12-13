module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra
import Debug.Trace
import Data.Maybe
import Control.Monad.State
import qualified Data.Map.Strict as Map

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ map processLine $ lines text
      _ -> putStrLn "Wrong number of arguments"

processLine line = let (result, found) =  runState (countPossibilities springs defectNums) Map.empty in result
 where
  springs = intercalate "?" $ replicate 5 $ head splitLine
  defectNums = concat $ replicate 5 $ map read $ splitOn "," $ last splitLine
  splitLine = splitOn " " line

countPossibilities::String->[Int]->State (Map.Map (String, [Int]) Int) Int
countPossibilities [] (_:_) = return 0
countPossibilities springs [] = if '#' `elem` springs then return 0 else return 1
countPossibilities springs@('.':remSprings) nums = countPossibilities' remSprings nums >>= updateFound springs nums
countPossibilities springs@('?':remSprings) nums = do
    tagResult <- checkForDefectGroup springs nums
    dotResult <- countPossibilities' remSprings nums
    updateFound springs nums (dotResult + tagResult)
countPossibilities springs@('#':_) nums@(num:remNums) = checkForDefectGroup springs nums

countPossibilities'::String->[Int]->State (Map.Map (String, [Int]) Int) Int
countPossibilities' springs nums = get >>= \found ->
  case Map.lookup (springs, nums) found of
    Nothing -> countPossibilities springs nums
    Just count -> return count

checkForDefectGroup::String->[Int]->State (Map.Map (String, [Int]) Int) Int
checkForDefectGroup springs nums@(num:remNums) = (if length springs >= num && all (`elem` "#?") (take num springs) && (length springs == num || (springs !! num) `elem` ".?") then countPossibilities' (drop (num + 1) springs) remNums else return 0) >>= updateFound springs nums

updateFound::String->[Int]->Int->State (Map.Map (String, [Int]) Int) Int
updateFound springs nums count = get >>= \found -> put (Map.insert (springs, nums) count found) >> return count
