module Main where
import System.Environment
import Data.List.Extra

type Pos = (Int, Int)

data Direction =  Haut | Droite | Bas | Gauche

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ foldr ((+).processLine) 0 (lines text)
      _ -> putStrLn "Wrong number of arguments"

processLine::String->Int
processLine input = let (goal, operands) = parseLine input in if findOperators goal operands then goal else 0

findOperators::Int->[Int]->Bool
findOperators goal (start:operands) = findOperators' goal operands start
  where
    findOperators' goal [] current = goal == current
    findOperators' goal (op:ops) current
      | current > goal = False
      | otherwise = findOperators' goal ops (current * op) || findOperators' goal ops (current + op) || findOperators' goal ops (read $ show current ++ show op) 

parseLine::String->(Int, [Int])
parseLine line = let [goal, operands] = splitOn ": " line in (read goal, map read $ splitOn " " operands)