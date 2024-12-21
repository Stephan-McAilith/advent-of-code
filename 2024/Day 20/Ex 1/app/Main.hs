module Main where
import System.Environment
import Data.Maybe
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Extra

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
countCheat fullPath = let trimed = drop 102 fullPath
                  in countCheat' fullPath trimed (S.fromList trimed)
  where
    countCheat' _ [] _ = 0
    countCheat' ((x,y):path) (t:trimed) targets = length (filter (\(dx,dy) -> S.member (dx+x,dy+y) targets) [(2,0),(0,2),(-2,0),(0,-2)]) + countCheat' path trimed (S.delete t targets)

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