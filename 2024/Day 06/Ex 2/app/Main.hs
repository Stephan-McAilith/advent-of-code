module Main where
import System.Environment
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Control.Monad.State

import Debug.Trace


data Direction =  Haut | Droite | Bas | Gauche deriving (Ord, Eq, Show)


type Pos = (Int,Int)
type Visited = Set.Set Pos

type DejaVu = Set.Set (Pos,Direction)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ process text
      _ -> putStrLn "Wrong number of arguments"

process::String->Int
process input = let plan = lines input
                    width = length $ head plan
                    heigth = length plan
                    start = findStart plan 0
                    path = followPath plan width heigth start Haut Set.empty
                    potentialObstacle = Set.toList $ Set.delete start path
                    in sum $ map (checkLoop plan width heigth start) potentialObstacle

followPath::[String]->Int->Int->Pos->Direction->Visited->Visited
followPath plan w h pos dir visited
  | x < 0 || x >= w || y < 0 || y >= h = updatedVisited
  | plan !! y !! x == '#' = followPath plan w h pos (turn dir) updatedVisited
  | otherwise =  followPath plan w h (x,y) dir updatedVisited
    where
      (x,y) = nextPos pos dir
      updatedVisited = Set.insert pos visited

checkLoop::[String]->Int->Int->Pos->Pos->Int
checkLoop plan w h start (x,y) = findLoop (addObstacle plan x y) w h start Haut Set.empty


findLoop::[String]->Int->Int->Pos->Direction->DejaVu->Int
findLoop plan w h pos dir dejaVu
  | Set.member (pos,dir) dejaVu = 1
  | x < 0 || x >= w || y < 0 || y >= h = 0
  | plan !! y !! x == '#' = findLoop plan w h pos (turn dir) updatedDejaVu
  | otherwise =  findLoop plan w h (x,y) dir updatedDejaVu
    where
      (x,y) = nextPos pos dir
      updatedDejaVu = Set.insert (pos, dir) dejaVu

addObstacle::[String]->Int->Int->[String]
addObstacle plan x y =  let end = drop y plan
                            mid = head end
                        in  take y plan ++ [(take x mid ++ "#" ++ drop (x+1) mid)]  ++ tail end


nextPos::Pos->Direction->Pos
nextPos (x,y) Haut = (x,y -1)
nextPos (x,y) Droite = (x + 1,y)
nextPos (x,y) Bas = (x,y + 1)
nextPos (x,y) Gauche = (x - 1,y)

turn::Direction->Direction
turn Haut = Droite
turn Droite = Bas
turn Bas = Gauche
turn Gauche = Haut

findStart::[String]->Int->Pos
findStart (r:rows) y = case elemIndex '^' r of
                        Just x -> (x, y)
                        Nothing -> findStart rows (y+1)



-- notVisited::Int->Int->Visited->Bool
-- notVisited x y visited = Set.null $ Set.filter (\(x',y',_) -> x' == x && y' == y) visited

 -- followPath plan w h pos visited obstacleRemain
--   | Set.member pos visited = trace (show pos) 1
--   | x < 0 || x >= w || y < 0 || y >= h = 0
--   | plan !! y !! x == '#' = followPath plan w h (turn pos) updatedVisited obstacleRemain
--   | obstacleRemain && notVisited x y updatedVisited = (followPath (addObstacle plan x y) w h (turn pos) updatedVisited False)  +  (followPath plan w h (x,y,dir) updatedVisited True)
--   | otherwise = followPath plan w h (x,y,dir) updatedVisited obstacleRemain
--   where
--     updatedVisited = Set.insert pos visited
--     (x,y,dir) = nextPos pos
