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
type Paths = S.Set (Int,Pos, [Pos])
type Visited = M.Map Pos Int

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        let (x,y) = process text
        putStrLn $ show x ++ "," ++ show y
      _ -> putStrLn "Wrong number of arguments"


process::String->Pos
process input = let bytes = map parseByte (lines input)
                in findBlockingByte bytes

parseByte::String->Pos
parseByte byte = let [x,y] = (map read . splitOn ",") byte in (x,y)

buildMaze::S.Set Pos->Maze
buildMaze bytes =  [row | y <- [0..70], let row = [if S.member (x,y) bytes then '#' else '.' | x <- [0..70]]]

findBlockingByte::[Pos]->Pos
findBlockingByte bytes = findBlockingByte' 1024
  where
    findBlockingByte' fallenBytes = let path = tryMaze fallenBytes
                                    in if S.null path then bytes !! (fallenBytes - 1)
                                       else findBlockingByte' (makeBytesFall path)
    makeBytesFall path = (snd . head . dropWhile ((`S.notMember` path) . fst)) zippedBytes
    zippedBytes = zip bytes [1..]
    tryMaze fallenBytes = solveMaze $ buildMaze (S.fromList $ take fallenBytes bytes)

solveMaze::Maze->S.Set Pos
solveMaze maze =  let start = (0,0)
                      end = (70, 70)
                  in findShortestPath maze start end

findShortestPath::Maze->Pos->Pos->S.Set Pos
findShortestPath maze start (ex, ey) = findShortestPath' (S.singleton (0, start, [start])) (M.singleton start 0)
  where
    findShortestPath'::Paths->Visited->S.Set Pos
    findShortestPath' paths seen =  case S.lookupMin paths of
                                      Nothing->S.empty
                                      Just shortest@(weight, (x,y), path)->
                                        if x == ex && y == ey then S.fromList path
                                        else  let newPaths = filter (\(w, pos, _) -> w < M.findWithDefault (maxBound::Int) pos seen) $ findAdjacentPath shortest
                                                  updatedSeen = foldr (\(w, pos, _) s -> M.insert pos w s) seen newPaths
                                              in findShortestPath' ((S.union (S.fromList newPaths) . S.delete shortest) paths) updatedSeen
    findAdjacentPath::(Int,Pos,[Pos])->[(Int,Pos,[Pos])]
    findAdjacentPath (weight, (x,y), path) = [(weight + 1, (x',y'), (x', y'):path) | (dx,dy) <- [(0,1), (1,0), (0,-1), (-1,0)], let x'=x+dx, let y'= y+dy, x' >= 0 && x' <= 70, y' >= 0 && y' <= 70, maze !! y' !! x' /= '#']

debug aled = trace (show aled) aled