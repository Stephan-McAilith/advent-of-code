module Main where
import System.Environment

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ processLines (lines text)
      _ -> putStrLn "Wrong number of arguments"

processLines::[String]->Int
processLines inputs = foldr ((+).findXmas) 0 (inputs ++ rotate90 inputs ++ rotate45 inputs ++ rotateMinus45 inputs)

findXmas::String->Int
findXmas [] = 0
findXmas input = let str = take 4 input in findXmas (tail input) + if str == "XMAS" || str == "SAMX" then 1 else 0

rotate90::[String]->[String]
rotate90 [] = []
rotate90 ([]:_) = []
rotate90 rows = map head rows : rotate90 (map tail rows)

rotate45::[String]->[String]
rotate45 [] = []
rotate45 (r:rows) = rotate45' [r] rows
  where
    rotate45'::[String]->[String]->[String]
    rotate45' [] _ = []
    rotate45' ([]:other) remains = rotate45' other remains
    rotate45' used [] = map head used : rotate45' (map tail used) []
    rotate45' used remains = map head used : rotate45' (map tail used ++ [head remains]) (tail remains)

rotateMinus45 = rotate45 . reverse