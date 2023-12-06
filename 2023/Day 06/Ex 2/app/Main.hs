module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Maybe
import Data.List.Extra

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (getWinPossibilities text)
      _ -> putStrLn "Wrong number of arguments"


getWinPossibilities::String->Int
getWinPossibilities text = case findMinPushingWin time record of
                            Just minTimePush ->  time - 2 * minTimePush + 1
                            Nothing -> 0
    where
        record = read $ filter isDigit (last (splitOn ":" distancessInput)) ::Int
        time =  read $ filter isDigit (last (splitOn ":" timesInput)) ::Int
        [timesInput, distancessInput] = lines text

findMinPushingWin::Int->Int->Maybe Int
findMinPushingWin = findMinPushingWin' 0
  where
    findMinPushingWin' pushedTime maxTime record
      | pushedTime `div` 2 > maxTime = Nothing
      | pushedTime * (maxTime - pushedTime) <= record = findMinPushingWin' (pushedTime + 1) maxTime record
      | otherwise = Just pushedTime