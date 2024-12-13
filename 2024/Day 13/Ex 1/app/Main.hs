module Main where
import System.Environment
import Data.List.Extra
import Data.Char (isDigit)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ foldr ((+).computeCoinsNeeded) 0 (parseClawMachines text)
      _ -> putStrLn "Wrong number of arguments"


parseClawMachines::String->[[(Int, Int)]]
parseClawMachines input = map (parseMachine . lines) (splitOn "\n\n" input)

computeCoinsNeeded::[(Int, Int)]->Int
computeCoinsNeeded [(ax, ay), (bx, by), (px, py)] = case [a * 3 + b | a <- [0..100], let tmpx = px -a * ax, let tmpy = py - a * ay, tmpx `mod` bx == 0, tmpy `mod` by == 0, let b = tmpx `div` bx, tmpy `div` by == b] of
                                                      [] -> 0
                                                      coins -> minimum coins

parseMachine::[String]->[(Int, Int)]
parseMachine = map parseXandY
  where
    parseXandY input =  let  (left, right) = break (==',') input
                        in (read (takeWhileEnd isDigit left), read (takeWhileEnd isDigit right))