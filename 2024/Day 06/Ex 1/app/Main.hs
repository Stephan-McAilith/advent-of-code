module Main where
import System.Environment
import Data.Maybe
import Data.List
import qualified Data.Set as Set

import Debug.Trace

type Pos = (Int, Int)

data Direction =  Haut | Droite | Bas | Gauche

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
                  in case followPath plan width heigth (findStart plan 0) Haut of
                      Nothing -> 0
                      Just visited -> Set.size visited

followPath::[String]->Int->Int->Pos->Direction->Maybe (Set.Set Pos)
followPath plan w h (x,y) dir
  | x < 0 || x >= w || y < 0 || y >= h = Just Set.empty
  | (plan !! y) !! x == '#' = Nothing
  | otherwise  =  Just $ Set.insert (x,y) $ case followPath plan w h (nextPos (x,y) dir) dir of
                  Nothing -> let  newDir = turn dir
                                  Just visited = followPath plan w h (nextPos (x,y) newDir) newDir in visited
                  Just visited -> visited

nextPos::Pos->Direction->Pos
nextPos (x,y) Haut = (x,y -1)
nextPos (x,y) Droite = (x + 1,y)
nextPos (x,y) Bas = (x,y + 1)
nextPos (x,y) Gauche= (x - 1,y)

turn::Direction->Direction
turn Haut = Droite
turn Droite = Bas
turn Bas = Gauche
turn Gauche = Haut

findStart::[String]->Int->Pos
findStart (r:rows) y = case elemIndex '^' r of
                        Just x -> (x, y)
                        Nothing -> findStart rows (y+1)