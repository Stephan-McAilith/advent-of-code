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
processLines = foldr ((+). checkReport) 0

checkReport::String->Int
checkReport line = let report = parseReport line; (x:y:_) = report in 
  if checkIsSafe (x < y) report then 1 else 0

checkIsSafe::Bool->[Int]->Bool
checkIsSafe _ [_] = True
checkIsSafe True (f:s:xs) = let diff = f-s in ((diff <= -1 && diff >= -3) && checkIsSafe True (s:xs))
checkIsSafe False (f:s:xs) = let diff = f-s in ((diff >= 1 && diff <= 3) && checkIsSafe False (s:xs))

parseReport::String->[Int]
parseReport line = map read $ splitOn " " line