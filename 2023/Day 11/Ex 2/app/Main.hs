module Main where
import System.Environment
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe
import Control.Concurrent (Chan)

type Galaxy = (Int,Int)

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ getDistances (lines text)
      _ -> putStrLn "Wrong number of arguments"


getDistances::[[Char]]->[Int]
getDistances image = computeDistances image 0 0 [] columnToExpand rowToExpand []
 where
    columnToExpand = mapMaybe (isColumnEmpty image)   [0..(length image - 1)]
    rowToExpand =  mapMaybe (uncurry isRowEmpty) (zip image [0..])

computeDistances::[[Char]]->Int->Int->[Galaxy]->[Int]->[Int]->[Int]->[Int]
computeDistances [] _ _ _ _ _ distances =  distances
computeDistances ([]:rows) y _ foundGalaxies columnToExpand rowToExpand distances = computeDistances rows (y+1) 0 foundGalaxies columnToExpand rowToExpand distances
computeDistances ((col:cols):rows) y x foundGalaxies columnToExpand rowToExpand distances
  | col == '.' = computeDistances (cols:rows) y (x+1) foundGalaxies columnToExpand rowToExpand distances
  | col == '#' = computeDistances (cols:rows) y (x+1) (newGalaxy:foundGalaxies) columnToExpand rowToExpand $ map (getDistanceFromGalaxy newGalaxy) foundGalaxies ++ distances
    where
      newGalaxy = (
                    let emptyRows = length (filter (< y) rowToExpand) in y - emptyRows + emptyRows * 1000000,
                    let emptyCols = length (filter (< x) columnToExpand) in x - emptyCols + emptyCols * 1000000
                  )

getDistanceFromGalaxy::Galaxy->Galaxy->Int
getDistanceFromGalaxy (y1,x1) (y2,x2) = abs (y1 - y2) + abs (x1 - x2)


isRowEmpty::[Char]->Int->Maybe Int
isRowEmpty row rowIndex= if all (=='.') row then Just rowIndex else Nothing

isColumnEmpty::[[Char]]->Int->Maybe Int
isColumnEmpty image columnIndex= if all ((== '.') . (!! columnIndex)) image then Just columnIndex else Nothing