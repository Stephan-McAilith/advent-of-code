module Main where
import System.Environment
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Bits

type KnowState = ([String], Int)

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ map countWeights $  (rotateClockWise . spinCycle . rotateClockWise . rotateClockWise) (lines text)
      _ -> putStrLn "Wrong number of arguments"

spinCycle::[String]->[String]
spinCycle rows = spinCycle' rows [] 0
  where
    spinCycle' rows _ 1000000000 =  rows
    spinCycle' rows knownStates cycleNumber = case find (\s -> fst s == rows) knownStates of
                                                Nothing -> spinCycle' (doCycle rows)  ((rows, cycleNumber):knownStates) $ cycleNumber + 1
                                                Just (_, matchedCycleNumber) -> spinCycle' rows [] (1000000000 -  ((1000000000 - cycleNumber) `mod` (cycleNumber - matchedCycleNumber)))

doCycle::[String]->[String]
doCycle = tiltMap . tiltMap . tiltMap . tiltMap

tiltMap::[String]->[String]
tiltMap rows = map roll $ rotateClockWise rows

roll::String->String
roll row = roll' row 0
  where
    roll' [] emptySpaces = replicate emptySpaces '.'
    roll' ('O':row) emptySpaces = 'O' : roll' row emptySpaces
    roll' ('.':row) emptySpaces = roll' row $ emptySpaces + 1
    roll' ('#':row) emptySpaces = replicate emptySpaces '.' ++ '#' : roll' row 0

rotateClockWise::[String]->[String]
rotateClockWise rows = let lengthRow = length $ head rows in map (\x -> reverse $ map (!! x) rows) [0..lengthRow - 1]


countWeights::String->Int
countWeights row = let lengthRow = length row in sum $ zipWith (\r v -> if r == 'O' then v else 0) row [lengthRow, lengthRow-1 .. 1]