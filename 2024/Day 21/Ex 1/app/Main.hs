module Main where
import System.Environment
import Data.Maybe
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace

type Pos = (Int, Int)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ foldr ((+) . processLine) 0 (lines text)
      _ -> putStrLn "Wrong number of arguments"

processLine::String->Int
processLine input = findSequenceLength input * (read . take 3) input

findSequenceLength = length . findSequenceFromDirectionCode . findSequenceFromDirectionCode . findSequenceFromDigiCode

findSequenceFromDigiCode::[Char]->[Char]
findSequenceFromDigiCode code = findSequenceFromDigiCode' code "" (digiCodePos 'A')
  where
      findSequenceFromDigiCode' [] sequence _ = sequence
      findSequenceFromDigiCode' (d:digits) sequence pos = let target = digiCodePos d
                                                              partialSequence = computeDigitSequence target pos
                                                            in findSequenceFromDigiCode' digits (sequence ++ partialSequence) target
computeDigitSequence::Pos->Pos->[Char]
computeDigitSequence (tx,ty) (x,y)
  | not (tx == 0 && y == 3) && not (ty == 3 && x == 0) = left (tx - x) ++ down (ty - y) ++ right (tx - x) ++ up (ty - y)  ++ "A"
  | not (tx == 0 && y == 3) = left (tx - x) ++ right (tx - x) ++ down (ty - y) ++ up (ty - y)  ++ "A"
  | not (ty == 3 && x == 0) = down (ty - y) ++ up (ty - y) ++ left (tx - x) ++ right (tx - x) ++ "A"
  | otherwise = up (ty - y) ++ right (tx - x) ++ left (tx - x) ++ down (ty - y) ++ "A"


findSequenceFromDirectionCode::[Char]->[Char]
findSequenceFromDirectionCode code = findSequenceFromDirectionCode' code "" (directionCodePos 'A')
  where
      findSequenceFromDirectionCode' [] sequence _ = sequence
      findSequenceFromDirectionCode' (d:digits) sequence pos = let  target = directionCodePos d
                                                                    partialSequence = computeDirectionSequence target pos
                                                                in findSequenceFromDirectionCode' digits (sequence ++ partialSequence) target

computeDirectionSequence::Pos->Pos->[Char]
computeDirectionSequence (tx,ty) (x,y)
  | not (tx == 0 && y == 0) = left (tx - x) ++ down (ty - y) ++ right (tx - x) ++ up (ty - y) ++ "A"
  | otherwise = down (ty - y) ++ left (tx - x) ++ right (tx - x) ++ up (ty - y) ++ "A"

up nb = if nb < 0 then replicate (abs nb) '^' else ""
down nb = if nb > 0 then replicate nb 'v' else ""
left nb = if nb < 0 then replicate (abs nb) '<' else ""
right nb = if nb > 0 then replicate nb '>' else ""


directionCodePos::Char->Pos
directionCodePos '^' = (1, 0)
directionCodePos 'A' = (2, 0)
directionCodePos '<' = (0, 1)
directionCodePos 'v' = (1, 1)
directionCodePos '>' = (2, 1)

digiCodePos::Char->Pos
digiCodePos '7' = (0, 0)
digiCodePos '8' = (1, 0)
digiCodePos '9' = (2, 0)
digiCodePos '4' = (0, 1)
digiCodePos '5' = (1, 1)
digiCodePos '6' = (2, 1)
digiCodePos '1' = (0, 2)
digiCodePos '2' = (1, 2)
digiCodePos '3' = (2, 2)
digiCodePos '0' = (1, 3)
digiCodePos 'A' = (2, 3)