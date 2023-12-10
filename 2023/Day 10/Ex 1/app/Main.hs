module Main where
import System.Environment
import Data.Char
import Data.List
import Debug.Trace

data Direction  = NORTH | SOUTH | WEST | EAST deriving (Eq, Show)
data Tile = PIPE Direction Direction
          | GROUND
          | START
          deriving (Eq)

getOppositeDir :: Direction -> Direction
getOppositeDir NORTH = SOUTH
getOppositeDir SOUTH = NORTH
getOppositeDir EAST = WEST
getOppositeDir WEST = EAST

hasDirection::Tile -> Direction -> Bool
hasDirection GROUND _ = False
hasDirection START _ = True
hasDirection (PIPE d1 d2) direction = d1 == direction || d2 == direction

findOnePipeAdjacentToStart::[[Tile]] -> (Int, Int) -> Direction
findOnePipeAdjacentToStart pipeMap@(row:_) (x, y)
  | x > 0 && pipeMap !! y !! (x-1) `hasDirection` EAST = WEST
  | y > 0 && pipeMap !! (y-1) !! x `hasDirection` SOUTH = NORTH
  | x < length row - 1 && pipeMap !! y !! (x+1) `hasDirection` WEST = EAST
  | y < length pipeMap - 1 && pipeMap !! (y+1) !! x `hasDirection` NORTH = SOUTH

getNextCoord::(Int, Int) -> Direction -> (Int, Int)
getNextCoord (x,y) dir
  | dir == NORTH = (x,y-1)
  | dir == SOUTH = (x,y+1)
  | dir == WEST = (x-1,y)
  | dir == EAST = (x+1,y)

followPipeLoop::[[Tile]] -> (Int, Int) -> Direction -> Int ->Int
followPipeLoop pipeMap pos@(x,y) fromDir steps =
  case pipeMap !! y !! x of
    START -> steps
    GROUND -> error "Should be impossible"
    (PIPE dir1 dir2) -> followPipeLoop pipeMap (getNextCoord pos nextDir) (getOppositeDir nextDir) $ steps + 1
      where
        nextDir = if dir1 == fromDir then dir2 else dir1

getLoopLength::[[Tile]]->(Int, Int) -> Int
getLoopLength pipeMap startCoord = followPipeLoop pipeMap (getNextCoord startCoord nextDir) (getOppositeDir nextDir) 1
  where
    nextDir = findOnePipeAdjacentToStart pipeMap startCoord

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ getFarthestPipe text
      _ -> putStrLn "Wrong number of arguments"

getFarthestPipe::String->Int
getFarthestPipe input = getLoopLength pipeMap startCoord `div` 2
  where
    pipeMap = map getPipes $ lines input
    startCoord = findStart pipeMap


findStart::[[Tile]]->(Int, Int)
findStart pipes = findStart' pipes 0 0
  where
    findStart' ([]:ys) x y = findStart' ys 0 $ y + 1
    findStart' ((START:xs):ys) x y = (x, y)
    findStart' ((_:xs):ys) x y = findStart' (xs:ys) (x + 1) y

getPipes :: [Char] -> [Tile]
getPipes = map getPipe

getPipe :: Char -> Tile
getPipe '|' = PIPE NORTH SOUTH
getPipe '-' = PIPE WEST EAST
getPipe 'J' = PIPE NORTH WEST
getPipe 'L' = PIPE NORTH EAST
getPipe '7' = PIPE SOUTH WEST
getPipe 'F' = PIPE SOUTH EAST
getPipe '.' = GROUND
getPipe 'S' = START