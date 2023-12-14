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
reflectionOf start end = case filter (/=0) $ zipWith xor mirroredStart truncatedEnd  of
                          [] -> False
                          [res] -> res `elem` sigleBitValues
                          _ -> False
  where
      minLength = min (length start) (length end)
      mirroredStart = take minLength $ reverse start
      truncatedEnd = take minLength end

computeRowValue::Int->String->Int
computeRowValue value [] = value
computeRowValue value ('.':row) = computeRowValue (shiftL value 1) row
computeRowValue value ('#':row) = computeRowValue (shiftL value 1 + 1) row

computeColValue::[String]->Int->Int
computeColValue rows colIndex = computeRowValue 0 $  map (!! colIndex) rows

sigleBitValues = [2^x | x <- [0..32]]