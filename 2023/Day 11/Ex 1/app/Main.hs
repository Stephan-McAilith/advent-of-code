module Main where
import System.Environment
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe

type Galaxy = (Int,Int)

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ getDistances $ expand (lines text)
      _ -> putStrLn "Wrong number of arguments"


getDistances::[[Char]]->[Int]
getDistances image = computeDistances image 0 0 [] []

computeDistances::[[Char]]->Int->Int->[Galaxy]->[Int]->[Int]
computeDistances [] _ _ _ distances = distances
computeDistances ([]:rows) y _ foundGalaxies distances = computeDistances rows (y+1) 0 foundGalaxies distances
computeDistances ((col:cols):rows) y x foundGalaxies distances
  | col == '.' = computeDistances (cols:rows) y (x+1) foundGalaxies distances
  | col == '#' = computeDistances (cols:rows) y (x+1) (newGalaxy:foundGalaxies) $ map (getDistanceFromGalaxy newGalaxy) foundGalaxies ++ distances
    where
      newGalaxy = (y,x)

getDistanceFromGalaxy::Galaxy->Galaxy->Int
getDistanceFromGalaxy (y1,x1) (y2,x2) = abs (y1 - y2) + abs (x1 - x2)

expand::[[Char]]->[[Char]]
expand image = expandRows (map (expandColumns columnToExpend) image)
  where
    columnToExpend = mapMaybe (isColumnEmpty image)   [0..(length image - 1)]

expandRows::[[Char]]->[[Char]]
expandRows [] = []
expandRows (row:rows)
 | all (=='.') row = row : row : expandRows rows
 | otherwise = row : expandRows rows

expandColumns::[Int]->[Char]->[Char]
expandColumns [col] row = take col row ++ '.' : drop col row
expandColumns (col1:col2:cols) row = let expandedRow = expandColumns (col2:cols) row in take col1 expandedRow ++ '.' : drop col1 expandedRow

isColumnEmpty::[[Char]]->Int->Maybe Int
isColumnEmpty image columnIndex= if all ((== '.') . (!! columnIndex)) image then Just columnIndex else Nothing