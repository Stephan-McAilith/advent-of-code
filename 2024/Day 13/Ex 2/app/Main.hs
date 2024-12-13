module Main where
import System.Environment
import Data.List.Extra
import Debug.Trace
import Data.Char (isDigit)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ foldr ((+).computeCoinsNeeded) 0 (parseClawMachines text)
      _ -> putStrLn "Wrong number of arguments"


parseClawMachines::String->[[(Double, Double)]]
parseClawMachines input = map (parseMachine . lines) (splitOn "\n\n" input)

computeCoinsNeeded::[(Double, Double)]->Int
computeCoinsNeeded [(ax, ay), (bx, by), (px, py)] =
  let
    mergedPrize =  ((px + 10000000000000) * ay) - ((py + 10000000000000) * ax)
    mergedB = (ay * bx) - (ax * by)
    b = mergedPrize / mergedB
    a = ((px + 10000000000000) - (bx * b)) / ax
  in if isInteger a && isInteger b then truncate (a * 3.0 + b) else 0
    where
      isInteger double = double == fromIntegral (truncate double)

parseMachine::[String]->[(Double, Double)]
parseMachine = map parseXandY
  where
    parseXandY input =  let  (left, right) = break (==',') input
                        in (read (takeWhileEnd isDigit left), read (takeWhileEnd isDigit right))