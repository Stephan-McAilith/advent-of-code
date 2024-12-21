module Main where
import System.Environment
import Data.Maybe
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Extra
import Debug.Trace

type Pos = (Int, Int)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        let path = followPath (lines text)
        print $ countCheat path
      _ -> putStrLn "Wrong number of arguments"


countCheat::[Pos]->Int
countCheat fullPath = countCheat' fullPath 0 (zip (drop 100 fullPath) [1..])
  where
    countCheat' _ _ [] = 0
    countCheat' ((x,y):path) i trimed = length (filter (\((tx,ty),index) -> let dist = (abs(tx - x) + abs(ty - y))  in  dist <= 20 && dist <= index - i) trimed) + countCheat' path (i+1)(tail trimed)

followPath::[[Char]]->[Pos]
followPath plan = let start = findStart plan in  followPath' start [start]
  where
    followPath' (x,y)  path@[start] = let next = (head [(x',y') | (dx, dy) <- [(1,0),(0,1),(-1,0),(0,-1)], let x'=x+dx, let y'=y+dy, plan !! y' !! x' /= '#'])
                                      in followPath' next (next:path)
    followPath' (x,y) path@(c:(px,py):rem)
      | plan !! y !! x == 'E' = reverse path
      | otherwise = let next = (head $ [(x',y') |(dx, dy) <- [(1,0),(0,1),(-1,0),(0,-1)], let x'=x+dx, let y'=y+dy, x'/=px || y'/=py , plan !! y' !! x' /= '#'])
                    in followPath' next (next:path)

findStart::[[Char]]->Pos
findStart input = findStart' input 0
  where
    findStart' (r:rows) y = case elemIndex 'S' r of
                        Just x -> (x, y)
                        Nothing -> findStart' rows (y+1)

debug aled = trace (show aled) aled