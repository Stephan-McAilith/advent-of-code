module Main where
import System.Environment
import Data.List.Extra
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Extra
type Pos = (Int, Int)

type Region = (Int, Int)
type Seen = Set.Set Pos

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ computePrice $ findRegions $ lines text
      _ -> putStrLn "Wrong number of arguments"

computePrice::[Region]->Int
computePrice = foldr ((+). uncurry (*)) 0

findRegions::[[Char]]->[Region]
findRegions plan = let (regions, seen) = runState (mapM (findRegion plan width height) [(x,y) | x <- [0..width-1], y <- [0..height-1]]) Set.empty in regions
  where
    height = length plan
    width = length $ head plan

findRegion::[[Char]]->Int->Int->Pos->State Seen Region
findRegion plan w h pos = do
  seen <- gets (Set.member pos)
  if seen then return (0,0)
  else findRegionDelimitations plan w h pos >>= \fences -> return (sum fences, length fences)

findRegionDelimitations::[[Char]]->Int->Int->Pos->State Seen [Int]
findRegionDelimitations plan w h (x,y) = do
    seen <- gets (Set.member (x,y))
    let crop = plan !! y !! x
    let surroundings = filter (\(a,b) -> isInBound (a,b) && (plan !! b !! a == crop)) $ map (addPos (x,y)) directions
    if seen then return []
    else modify (Set.insert (x,y)) >> concatMapM (findRegionDelimitations plan w h) surroundings >>= \fences -> return ((4 - length surroundings):fences)
  where
    isInBound (a,b) = a >= 0 && a < w && b >= 0 && b < h
 
directions = [(-1,0), (1,0), (0,1), (0,-1)]
addPos (x,y) (x',y') = (x+x', y+y')