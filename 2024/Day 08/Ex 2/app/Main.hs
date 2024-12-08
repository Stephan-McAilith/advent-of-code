module Main where
import System.Environment
import qualified Data.Map.Strict as Map
import Data.List
import qualified Data.Set as Set

type Pos = (Int, Int)
type AntennasMap = Map.Map Char [Pos]


main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ findAntinodes (lines text)
      _ -> putStrLn "Wrong number of arguments"

findAntinodes::[String]->Int
findAntinodes plan =  let antennas = findAntennas plan
                          width = length $ head plan
                          height = length plan
                      in Set.size $ Map.foldr' (Set.union . checkAntinodes width height) Set.empty antennas

checkAntinodes::Int->Int->[Pos]->Set.Set Pos
checkAntinodes w h positions = Set.fromList $ concatMap (countAntinodes w h) [(a1, a2) | (a1:ps) <- tails positions, a2 <- ps]

countAntinodes::Int->Int->(Pos,Pos)->[Pos]
countAntinodes w h ((a,b),(a',b')) =  let (x,y) = (a-a', b-b')
                                          antinodeBefore = takeWhile checkInBound [(a+(x*i), b+(y*i)) | i <- [0..]]
                                          antinodeAfter = takeWhile checkInBound [(a-(x*i), b-(y*i)) | i <- [1..]]
                                      in antinodeBefore ++ antinodeAfter
  where
    checkInBound (i,j) =  not $ i < 0 || i >= w || j < 0 || j >= h

findAntennas::[String]->AntennasMap
findAntennas plan = findAntennas' plan 0 Map.empty
  where
    findAntennas' [] _ antennas = antennas
    findAntennas' (row:rows) y antennas = let newAntennas = [(f,(x,y)) | (x, f) <- zip [0..] row, f /= '.']
                                          in findAntennas' rows (y+1) $ foldr (\(f,pos) mp -> Map.insertWith (++) f [pos] mp) antennas newAntennas