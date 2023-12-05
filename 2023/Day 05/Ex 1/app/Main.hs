module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra

data Range = Range Int Int Int



findDestination::Int->[Range]->Int
findDestination src [] = src
findDestination src ((Range destRange srcRange range):ranges)
  | src < srcRange || src > srcRange + (range -1) = findDestination src ranges
  | otherwise = destRange + src - srcRange

convert::[Int]->[Range]->[Int]
convert [] _ = []
convert (src:rem) ranges = findDestination src ranges : convert rem ranges

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (minimum (findLocations text))
      _ -> putStrLn "Wrong number of arguments"


findLocations :: String -> [Int]
findLocations input = foldl convert seeds converterMaps
  where
    splitedInput = splitOn "\n\n" input
    seeds = parseInts (dropWhile (not.isDigit) (head splitedInput))
    converterMaps = map (getConverters . tail . lines) (tail splitedInput) 

getConverters::[String]->[Range]
getConverters [] = []
getConverters (line:rem) = Range dest src range : getConverters rem
  where
    [dest,src,range] = parseInts line

parseInts::String->[Int]
parseInts str = map read (words str)
