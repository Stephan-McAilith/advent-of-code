module Main where
import System.Environment
import Data.List
import Data.Char (digitToInt)
import qualified Data.Set as Set

type Pos = (Int, Int)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ countTrailheads (map (map digitToInt) (lines text))
      _ -> putStrLn "Wrong number of arguments"

countTrailheads::[[Int]]->Int
countTrailheads topography =  let height = length topography
                                  width = length $ head topography
                              in foldr ((+) . findTrailheads topography width height) 0 [(x,y) | x <- [0..(width-1)] ,y <- [0..(height-1)], topography !! y !! x == 0]

findTrailheads::[[Int]]->Int->Int->(Int,Int)->Int
findTrailheads topography width height (x,y) = let topo = topography !! y !! x
                                        in if topo == 9 then 1
                                         else sum [findTrailheads' (x', y') | (i,j) <- directions, let x' = x + i, let y' = y + j, isInBound x' y', topography !! y' !! x' == topo + 1]
  where
    findTrailheads' = findTrailheads topography width height
    isInBound a b = a >= 0 && a < width && b >= 0 && b < height

directions = [(-1,0), (1,0), (0,1), (0,-1)]