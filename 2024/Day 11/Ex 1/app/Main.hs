module Main where
import System.Environment
import Data.List.Extra
import qualified Data.Map.Strict as Map

type Stones = Map.Map Int Int

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $  countStones $ map read (splitOn " " text)
      _ -> putStrLn "Wrong number of arguments"

countStones::[Int]->Int
countStones stones = blinks 25 (foldr (\stone -> Map.insertWith (+) stone 1)  Map.empty stones)

blinks::Int->Stones->Int
blinks 0 stones = Map.foldr (+) 0 stones
blinks remaningBlinks stones = blinks (remaningBlinks - 1) $ foldr (factoriseStones . (\(s,n) -> (n,blink s))) Map.empty (Map.assocs stones)

factoriseStones::(Int,[Int])->Stones->Stones
factoriseStones (n,[s]) stones = Map.insertWith (+) s n stones
factoriseStones (n,[s1, s2]) stones = (Map.insertWith (+) s2 n . Map.insertWith (+) s1 n) stones

blink::Int->[Int]
blink 0 = [1]
blink stone = let stoneChars = show stone
                  len = length stoneChars
                  half = len `div` 2
              in  if even len then [(read . take half) stoneChars, (read . drop half) stoneChars]
                  else [stone * 2024]
