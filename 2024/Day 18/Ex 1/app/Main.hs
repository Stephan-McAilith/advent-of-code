module Main where
import System.Environment
import Data.Char (isDigit)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.List.Extra
import Data.Maybe
import Debug.Trace

type Maze = [[Char]]
type Pos = (Int, Int)
type Paths = S.Set (Int,Pos)
type Visited = M.Map Pos Int

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ process text
      _ -> putStrLn "Wrong number of arguments"


process::String->Int
process input = let bytes = (S.fromList . map parseByte . take 1024) (lines input)
                    maze = buildMaze bytes
                in  solveMaze maze

parseByte::String->Pos         
parseByte byte = let [x,y] = (map read . splitOn ",") byte in (x,y)

buildMaze::S.Set Pos->Maze
buildMaze bytes =  [row | y <- [0..70], let row = [if S.member (x,y) bytes then '#' else '.' | x <- [0..70]]]

solveMaze::Maze->Int
solveMaze maze =  let start = (0,0)
                      end = (70, 70)
                  in findShortestPath maze start end

findShortestPath::Maze->Pos->Pos->Int
findShortestPath maze start (ex, ey) = findShortestPath' (S.singleton (0, start)) (M.singleton start 0)
  where
    findShortestPath'::Paths->Visited->Int
    findShortestPath' paths seen =  let shortest@(weight, (x,y)) = S.findMin paths
                                    in  if x == ex && y == ey then weight
                                        else let  newPaths = filter (\(w, p) -> w < M.findWithDefault (maxBound::Int) p seen) $ findAdjacentPath shortest
                                                  updatedSeen = foldr (\(w, p) s -> M.insert p w s) seen newPaths 
                                             in findShortestPath' ((S.union (S.fromList newPaths) . S.delete shortest) paths) updatedSeen
    findAdjacentPath::(Int,Pos)->[(Int,Pos)]
    findAdjacentPath (weight, (x,y)) = [(weight + 1, (x', y')) | (dx,dy) <- [(0,1), (1,0), (0,-1), (-1,0)], let x'=x+dx, let y'= y+dy, x' >= 0 && x' <= 70, y' >= 0 && y' <= 70, maze !! y' !! x' /= '#']
