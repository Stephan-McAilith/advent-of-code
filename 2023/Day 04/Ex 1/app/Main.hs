module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print  (sum (map  processLine (lines text)))
      _ -> putStrLn "Wrong number of arguments"


processLine :: String ->  Int
processLine str = case filter (`elem` winningNumbers) myNumbers of
                  [] -> 0
                  goodNumbers -> 2 ^ (length goodNumbers - 1)
  where
    numbers = splitOn "|" (last (splitOn ":" str))
    winningNumbers = parseInts (head numbers)
    myNumbers = parseInts (last numbers)

parseInts::String->[Int]
parseInts str = map read (words str)
