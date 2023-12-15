module Main where
import System.Environment
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Bits

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ map (countWeight . roll) $ parseMap text
      _ -> putStrLn "Wrong number of arguments"

parseMap input = let
  lengthRow = length $ takeWhile (/= '\n') input
  rows = lines input
  in map (\x -> map (!! x) rows) [0..lengthRow - 1]

roll::String->String
roll row = roll' row 0
  where
    roll' [] emptySpaces = replicate emptySpaces '.'
    roll' ('O':row) emptySpaces = 'O' : roll' row emptySpaces
    roll' ('.':row) emptySpaces = roll' row $ emptySpaces + 1
    roll' ('#':row) emptySpaces = replicate emptySpaces '.' ++ '#' : roll' row 0

countWeight::String->Int
countWeight row = let lengthRow = length row in sum $ zipWith (\r v -> if r == 'O' then v else 0) row [lengthRow, lengthRow-1 .. 1]