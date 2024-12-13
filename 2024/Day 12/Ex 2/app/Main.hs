module Main where
import System.Environment
import Data.List.Extra
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Extra
import Debug.Trace


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

computePrice::[[Pos]]->Int
computePrice = foldr ((+) . computeRegionPrice) 0

computeRegionPrice::[Pos]->Int
computeRegionPrice [] = 0
computeRegionPrice positions =  sides * length positions
  where
    minX = foldr (\(x,y) currentMin -> min x currentMin) (maxBound::Int) positions
    minY = foldr (\(x,y) currentMin -> min y currentMin) (maxBound::Int) positions
    maxX = foldr (\(x,y) currentMax -> max x currentMax) 0 positions
    maxY = foldr (\(x,y) currentMax -> max y currentMax) 0 positions
    sidesHorizontal = countSidesHorizontal positions [] minY (+1) (<=maxY) + countSidesHorizontal positions [] maxY (\a->a-1) (>= minY)
    sidesVertical = countSidesVertical positions [] minX (+1) (<=maxX) + countSidesVertical positions [] maxX (\a->a-1) (>= minX)
    sides = sidesVertical + sidesHorizontal

countSidesHorizontal::[Pos]->[Int]->Int->(Int->Int)->(Int->Bool)->Int
countSidesHorizontal positions previous currentY incr continue
  | (not . continue) currentY = 0
  | otherwise =
    let filled = map fst $ filter (\(_,y)->y == currentY) positions
        edges = sort $ filter (not . (`elem` previous)) filled
    in  length (groupByPrev (\a b -> b - a == 1) edges) + countSidesHorizontal positions filled (incr currentY) incr continue

countSidesVertical::[Pos]->[Int]->Int->(Int->Int)->(Int->Bool)->Int
countSidesVertical positions previous currentX incr continue
  | (not . continue) currentX = 0
  | otherwise =
    let filled = map snd $ filter (\(x,_)->x == currentX) positions
        edges = sort $ filter (not . (`elem` previous)) filled
    in  length (groupByPrev (\a b -> b - a == 1) edges) + countSidesVertical positions filled (incr currentX) incr continue


groupByPrev::(Int->Int->Bool)->[Int]->[[Int]]
groupByPrev _ [] = []
groupByPrev _ [x] = [[x]]
groupByPrev cmp (x:xs) = go x [x] xs
  where
    go _ acc [] = [acc]
    go prev acc (y:ys)
      | cmp prev y = go y (y:acc) ys
      | otherwise = acc : go y [y] ys

findRegions::[[Char]]->[[Pos]]
findRegions plan = let (regions, seen) = runState (mapM (findRegionArea plan width height) [(x,y) | x <- [0..width-1], y <- [0..height-1]]) Set.empty in regions
  where
    height = length plan
    width = length $ head plan

findRegionArea::[[Char]]->Int->Int->Pos->State Seen [Pos]
findRegionArea plan w h (x,y) = do
    seen <- gets (Set.member (x,y))
    let crop = plan !! y !! x
    let surroundings = filter (\(a,b) -> isInBound (a,b) && (plan !! b !! a == crop)) $ map (addPos (x,y)) directions
    if seen then return []
    else modify (Set.insert (x,y)) >> concatMapM (findRegionArea plan w h) surroundings >>= \fences -> return ((x,y):fences)
  where
    isInBound (a,b) = a >= 0 && a < w && b >= 0 && b < h

directions = [(-1,0), (1,0), (0,1), (0,-1)]
addPos (x,y) (x',y') = (x+x', y+y')

debug aled = trace (show aled) aled