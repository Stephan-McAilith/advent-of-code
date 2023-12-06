module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Maybe
import Data.List.Extra

data Race = Race Int Int

instance Show Race where
    show (Race time record) = "Race " ++ show time ++ " " ++ show record ++ "\n"

getWinPossibilities::Race -> Int
getWinPossibilities (Race time record) = length $ filter (> record) (zipWith (*) [0..time] $ reverse [0..time])

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ product (map getWinPossibilities $ getRaces text)
      _ -> putStrLn "Wrong number of arguments"


getRaces::String->[Race]
getRaces text = buildRace times records
    where
        records = parseInts (last (splitOn ":" distancessInput))
        times = parseInts (last (splitOn ":" timesInput))
        [timesInput, distancessInput] = lines text

buildRace::[Int]->[Int]->[Race]
buildRace [] [] = []
buildRace (t:times) (r:records) = Race t r : buildRace times records

parseInts::String->[Int]
parseInts str = map read (words str)
