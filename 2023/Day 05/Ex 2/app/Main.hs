module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra
import Data.Maybe

data Range = ConverterRange Int Int Int
          | SeedRange Int Int


instance Show Range where
    show (ConverterRange src dest range) = "ConverterRange " ++ show src ++ " " ++ show dest ++ " " ++ show range ++ "\n"
    show (SeedRange src range) = "SeedRange " ++ show src ++ " "  ++ show range ++ "\n"


searchCrossingRange::Range->[Range]-> Maybe Range
searchCrossingRange _ [] = Nothing
searchCrossingRange (SeedRange startRange endRange) (ConverterRange dest src range : ranges)
  | startRange > src + range - 1 || endRange < src = searchCrossingRange (SeedRange startRange endRange) ranges
  | otherwise = Just  (ConverterRange dest src range)

convertSeedRange::Range->Range->(Range, [Range])
convertSeedRange (SeedRange startRange endRange) (ConverterRange dest src range) = (convertedSeedRange, remSeedRange)
  where
    convertedSeedRange = SeedRange (dest + containedRangeStart - src) (dest +  containedRangeEnd - src)
    containedRangeStart =  max startRange src 
    containedRangeEnd =  min endRange (src + range - 1)
    remSeedRange
      | containedRangeStart == startRange && containedRangeEnd == endRange = []
      | containedRangeStart > startRange && containedRangeEnd == endRange = [SeedRange startRange (containedRangeStart - 1)]
      | containedRangeStart > startRange && containedRangeEnd < endRange = [SeedRange startRange (containedRangeStart - 1), SeedRange (containedRangeEnd + 1) endRange]
      | containedRangeStart == startRange && containedRangeEnd < endRange = [ SeedRange (containedRangeEnd + 1) endRange]


convert::[Range]->[Range]->[Range]
convert [] _ = []
convert (seedRange:rem) ranges = case searchCrossingRange seedRange ranges of
                                  Nothing -> seedRange : convert rem ranges
                                  Just crossingRange -> convertedSeedRange : convert  (remSeedRange ++ rem) ranges
                                    where
                                      (convertedSeedRange, remSeedRange) = convertSeedRange seedRange crossingRange

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (minimum (findLocations text))
      _ -> putStrLn "Wrong number of arguments"


findLocations :: String -> [Int]
findLocations input = map (\(SeedRange start _) -> start ) (foldl convert seedRanges converterMaps)
  where
    splitedInput = splitOn "\n\n" input
    seedRanges = getSeedRanges (parseInts (dropWhile (not.isDigit) (head splitedInput)))
    converterMaps = map (getConverters . tail . lines) (tail splitedInput)

getSeedRanges::[Int]->[Range]
getSeedRanges [] = []
getSeedRanges (src:(range:rem)) = SeedRange src (src + range - 1) : getSeedRanges rem

getConverters::[String]->[Range]
getConverters [] = []
getConverters (line:rem) = ConverterRange dest src range : getConverters rem
  where
    [dest,src,range] = parseInts line

parseInts::String->[Int]
parseInts str = map read (words str)
