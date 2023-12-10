module Main where
import System.Environment
import Data.Char
import Data.List
import Debug.Trace

--Not proud of this one but I had other things to do on my sunday

data Direction  = NORTH | SOUTH | WEST | EAST deriving (Eq, Show)
data Tile = PIPE Direction Direction
          | GROUND
          | START
          deriving (Eq, Show)
data Path = Path { coord::Point, direction::Direction } deriving (Show)

type Point = (Int, Int)

getOppositeDir :: Direction -> Direction
getOppositeDir NORTH = SOUTH
getOppositeDir SOUTH = NORTH
getOppositeDir EAST = WEST
getOppositeDir WEST = EAST

hasDirection::Tile -> Direction -> Bool
hasDirection GROUND _ = False
hasDirection START _ = True
hasDirection (PIPE d1 d2) dir = d1 == dir || d2 == dir

findOnePipeAdjacentToStart::[[Tile]] -> Point -> Direction
findOnePipeAdjacentToStart pipeMap@(row:_) (x, y)
  | x > 0 && pipeMap !! y !! (x-1) `hasDirection` EAST = WEST
  | y > 0 && pipeMap !! (y-1) !! x `hasDirection` SOUTH = NORTH
  | x < length row - 1 && pipeMap !! y !! (x+1) `hasDirection` WEST = EAST
  | y < length pipeMap - 1 && pipeMap !! (y+1) !! x `hasDirection` NORTH = SOUTH

getNextCoord::Point -> Direction -> Point
getNextCoord (x,y) dir
  | dir == NORTH = (x,y-1)
  | dir == SOUTH = (x,y+1)
  | dir == WEST = (x-1,y)
  | dir == EAST = (x+1,y)

followPipeLoop::[[Tile]] -> Point -> Direction -> [Path] -> [Path]
followPipeLoop pipeMap pos@(x,y) fromDir path =
  case pipeMap !! y !! x of
    START -> path
    GROUND -> error "Should be impossible"
    (PIPE dir1 dir2) -> followPipeLoop pipeMap (getNextCoord pos nextDir) (getOppositeDir nextDir) $ path ++ [Path pos nextDir]
      where
        nextDir = if dir1 == fromDir then dir2 else dir1

getLoopPath::[[Tile]]->Point -> [Path]
getLoopPath pipeMap startCoord = followPipeLoop pipeMap (getNextCoord startCoord nextDir) (getOppositeDir nextDir) [Path startCoord nextDir]
  where
    nextDir = findOnePipeAdjacentToStart pipeMap startCoord

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ getContainedTiles text
      _ -> putStrLn "Wrong number of arguments"

getContainedTiles::String-> Int
getContainedTiles input = sum $ zipWith (curry (countTileInside polygonePoints startPipe)) pipeMap [0..]
  where
    pipeMap = map getPipes $ lines input
    startCoord = findStart pipeMap
    startPipe = PIPE ((direction . head)loopPath) ((getOppositeDir . direction . last) loopPath)
    polygonePoints = map coord  $ getLoopPath pipeMap startCoord
    loopPath = getLoopPath pipeMap startCoord

countTileInside::[(Int, Int)]-> Tile ->([Tile],Int)->Int
countTileInside polygonePoints startTile (row,rowId) = countTileInside' row False 0
  where
    countTileInside' [] _ _ = 0
    countTileInside' (tile:tiles) isInside x
      | x `notElem` filteredPoints = (if isInside then 1 else 0) +  countTileInside' tiles isInside (x + 1)
      | otherwise = countTileInside' tiles (computeIsInside isInside tile startTile) (x + 1)
    filteredPoints = map fst $ filter ((== rowId) . snd) polygonePoints

computeIsInside::Bool->Tile->Tile->Bool
computeIsInside isInside START startTile = computeIsInside isInside startTile startTile
computeIsInside isInside (PIPE NORTH SOUTH) _ = not isInside
computeIsInside isInside (PIPE SOUTH _) _ = not isInside
computeIsInside isInside _ _ =  isInside

findStart::[[Tile]]->Point
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