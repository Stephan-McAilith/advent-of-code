module Main where
import System.Environment
import Data.Char (isDigit)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe

type Maze = [[Char]]
type Pos = (Int, Int)
type Dir = (Int, Int)
type Paths = S.Set (Int,Pos,Dir)
type Visited = M.Map (Pos,Dir) Int

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ solveMaze (lines text)
      _ -> putStrLn "Wrong number of arguments"

solveMaze::Maze->Int
solveMaze maze =  let start = (1, length maze - 2)
                      end = (length (head maze) - 2, 1)
                  in findShortestPath maze start end

findShortestPath::Maze->Pos->Dir->Int
findShortestPath maze start (ex, ey) = findShortestPath' (S.singleton (0, start, (1, 0))) (M.singleton  (start, (1, 0)) 0)
  where
    findShortestPath'::Paths->Visited->Int
    findShortestPath' paths seen =  let shortest@(weight, (x,y), (dx,dy)) = S.findMin paths
                                    in  if x == ex && y == ey then weight
                                        else let  newPaths = filter (\(w, p, d) -> w < M.findWithDefault (maxBound::Int) (p,d) seen) $ findAdjacentPath shortest
                                                  updatedSeen = foldr (\(w, p, d) s -> M.insert (p,d) w s) seen newPaths 
                                             in findShortestPath' ((S.union (S.fromList newPaths) . S.delete shortest) paths) updatedSeen

    findAdjacentPath (weight, pos@(x,y), dir@(dx,dy)) = let rotations = [(weight + 1000, pos, rotation) | rotation@(rx,ry) <- [(0,1), (1,0), (0,-1), (-1,0)], rotation /= dir, maze !! (y+ry) !! (x+rx) /= '#']
                                                        in  if maze !! (y+dy) !! (x+dx) == '#' then rotations
                                                            else (weight + 1, (x+dx, y+dy), dir):rotations