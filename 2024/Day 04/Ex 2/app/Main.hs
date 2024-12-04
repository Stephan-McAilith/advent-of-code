module Main where
import System.Environment

type XMas = (Int,Int)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ process (lines text)
      _ -> putStrLn "Wrong number of arguments"

process::[String]->Int
process inputs = length (filter (`elem` revertDiagResult) diagResult)
  where
    width = length $ head inputs
    diagResult = findXmas (rotate45 inputs) width
    revertDiagResult = map (normalise width) $ findXmas (reverse (map reverse (rotateMinus45 inputs))) width

normalise::Int->XMas->XMas
normalise width (x,y) = (width - x - 1, y)

findXmas::[String]->Int->[XMas]
findXmas inputs width = findXmas' inputs 0
  where
    findXmas' [] _ = []
    findXmas' (l:ls) x
      | x < width = findMas l x 0 ++ findXmas' ls (x+1)
      | otherwise = findMas l (width - 1) (x - width + 1) ++ findXmas' ls (x+1)

findMas::String->Int->Int->[XMas]
findMas [] _ _ = []
findMas input x y = let str = take 3 input in
  findMas (tail input) (x - 1) (y + 1) ++ ([(x - 1,y + 1) | str == "MAS" || str == "SAM"])


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