module Main where
import System.Environment
import Data.List.Extra
import Debug.Trace

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ processLines (lines text)
      _ -> putStrLn "Wrong number of arguments"

processLines::[String]->Int
processLines = foldr ((+) . checkReport) 0

checkReport::String->Int
checkReport line = let levelDiffs = computeDiff (parseReport line) in if checkIfSafe 1 levelDiffs || checkIfSafe 1 (map (* (-1)) levelDiffs) then 1 else 0

checkIfSafe::Int->[Int]->Bool
checkIfSafe (-1) _ = False
checkIfSafe dampeners [x]  = (x <= 3 && x >= 1) || dampeners == 1
-- checkIfSafe Fasle [x] dampeners = (x >= -3 && x <= -1) || dampeners == 1
checkIfSafe dampeners (x:y:diffs)
  | x > 3 || x < 1 = checkIfSafe (dampeners - 1) (y:diffs)  || checkIfSafe (dampeners - 1) ((x+y):diffs)
  | otherwise = checkIfSafe' dampeners (x:y:diffs)
-- checkIfSafe Fasle dampeners (x:y:diffs) 
--   | x < -3 || x > -1 = checkIfSafe True (dampeners - 1) (y:diffs)  || checkIfSafe True (dampeners - 1) ((x+y):diffs)
--   | otherwise = checkIfSafe' True dampeners (x:y:diffs)

checkIfSafe'::Int->[Int]->Bool
checkIfSafe' (-1) _ = False
checkIfSafe' dampeners [x,y] = (y <= 3 && y >= 1) || dampeners == 1
checkIfSafe' dampeners (x:y:z:diffs)
  | y > 3 || y < 1 = checkIfSafe (dampeners - 1) (x:(y+z):diffs)  || checkIfSafe (dampeners - 1) ((x+y):z:diffs)
  | otherwise = checkIfSafe' dampeners (y:z:diffs)
-- checkIfSafe Fasle dampeners (x:y:z:diffs) 
--   | y < -3 || y > -1 = checkIfSafe True (dampeners - 1) (x:(y+z):diffs)  || checkIfSafe True (dampeners - 1) ((x+y):z:diffs)
--   | otherwise = checkIfSafe' True dampeners (y:z:diffs)

computeDiff::[Int]->[Int]
computeDiff [_] = []
computeDiff (x:y:xs) = (x-y) : computeDiff (y:xs)

parseReport::String->[Int]
parseReport line = map read $ splitOn " " line