module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra
import Debug.Trace
import Data.Maybe
import Data.Bits

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ map findReflection $ splitOn "\n\n" text
      _ -> putStrLn "Wrong number of arguments"

findReflection::String->Int
findReflection chunk = case searchForReflection rows of
                        Just mirrorIndex -> 100 * mirrorIndex
                        Nothing ->  case searchForReflection cols of
                          Just mirrorIndex -> mirrorIndex
                          Nothing -> error "Should be impossible"
  where
    rows = map (computeRowValue 0) splitChunk
    cols = map (computeColValue splitChunk) [0 .. length (head splitChunk) - 1]
    splitChunk = lines chunk

searchForReflection::[Int]->Maybe Int
searchForReflection values = case mapMaybe (\splitIndex -> if take splitIndex values `reflectionOf` drop splitIndex values then Just splitIndex else Nothing) [1 .. length values - 1] of
                                    [] -> Nothing
                                    [mirrorIndex] -> Just mirrorIndex

reflectionOf::[Int]->[Int]->Bool
reflectionOf start end = let mirroredStart = reverse start in strncmp (min (length mirroredStart) (length end)) mirroredStart end

strncmp ::Ord a => Int -> [a] -> [a] -> Bool
strncmp n s1 s2 = take n s1 == take n s2

computeRowValue::Int->String->Int
computeRowValue value [] = value
computeRowValue value ('.':row) = computeRowValue (shiftL value 1) row
computeRowValue value ('#':row) = computeRowValue (shiftL value 1 + 1) row

computeColValue::[String]->Int->Int
computeColValue rows colIndex = computeRowValue 0 $  map (!! colIndex) rows