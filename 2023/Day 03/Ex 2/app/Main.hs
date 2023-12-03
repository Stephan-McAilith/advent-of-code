module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Maybe (isJust, catMaybes, mapMaybe)

data Parser d = Parsed d String

data Part = PartNumber Int Int Int
          | Gear Int Int

instance Show Part where
    show (Gear y x) = "Gear " ++ show x ++ " " ++ show y
    show (PartNumber y x v) = "PartNumber " ++ show x ++ " " ++ show y ++ " "++ show v

isAdjacentPartNumber:: Part ->Part -> Bool
isAdjacentPartNumber _ (Gear {} ) = False
isAdjacentPartNumber (Gear gy gx) (PartNumber py px pv ) =  gy `elem` [(py - 1)..(py + 1)] && gx `elem` [px-1..(px + valueLength)]
  where
      valueLength = getPowerOf10 pv + 1

getGearRatio:: [Part] -> Part -> Maybe Int
getGearRatio _ (PartNumber {} ) = Nothing
getGearRatio parts gear = case filter (isAdjacentPartNumber gear) parts of
                          [PartNumber _ _ v1, PartNumber _ _ v2] -> Just (v1 * v2)
                          _ -> Nothing

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (sum (findPartNumbers (lines text)))
      _ -> putStrLn "Wrong number of arguments"

findPartNumbers::[String]->[Int]
findPartNumbers schematic = mapMaybe (getGearRatio parts) parts
  where parts = getAllNumbers schematic

getAllNumbers::[String]->[Part]
getAllNumbers schematic = concatMap getAllNumbersOnLine (zip schematic [0..])

getAllNumbersOnLine::(String,Int)->[Part]
getAllNumbersOnLine (line, y) = getAllNumbersOnLine' y 0
  where
    getAllNumbersOnLine' y x = case findNextPart (drop x line) y x of
                                    Nothing-> []
                                    Just (PartNumber _ foundX value) -> PartNumber y foundX value : getAllNumbersOnLine' y (foundX + getPowerOf10 value + 1)
                                    Just (Gear _ foundX) -> Gear y foundX: getAllNumbersOnLine' y (foundX + 1)

findNextPart::String->Int->Int->Maybe Part
findNextPart [] y x = Nothing
findNextPart ('*':line) y x = Just (Gear y x)
findNextPart line y x = if isDigit (head line) then Just (PartNumber y x (parseInt line)) else findNextPart (tail line) y (x + 1)

get::[String]->Int->Int->Maybe Char
get schematic y x = if y >= 0 && y < height && x >= 0 && x < width then Just (schematic !! y !! x) else Nothing
  where
    height = length schematic
    width = length (head schematic)

parseInt::String->Int
parseInt str = read (takeWhile isDigit str)::Int

getPowerOf10::Int->Int
getPowerOf10 value = (floor . logBase 10 . fromInteger) (toInteger  value)
