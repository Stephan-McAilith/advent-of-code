module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Maybe (isJust, catMaybes)

data Parser d = Parsed d String

data Part = PartNumber Int Int Int

isPartNumberSymbol :: Char -> Bool
isPartNumberSymbol c = (not . isDigit) c && c /= '.'

doubleMap::(a->b->c) -> [a]->[b]->  [c]
doubleMap _ [] _= []
doubleMap _ _ [] = []
doubleMap fun (y:ys) (x:xs)  = fun y x : doubleMap fun (y:ys) xs  ++ doubleMap fun ys (x:xs) 

getSurroundingChars :: [String] -> Int -> Int -> Int->  [Char]
getSurroundingChars schematic y x valueLength = catMaybes  (doubleMap (get schematic) [y-1..y+1] [x-1..(x+valueLength)])

isValidPartNumber::[String]->Part->Bool
isValidPartNumber schematic (PartNumber y x value) = any isPartNumberSymbol (getSurroundingChars schematic y x valueLength)
  where
    valueLength = getPowerOf10 value + 1

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (sum(findPartNumbers (lines text)))
      _ -> putStrLn "Wrong number of arguments"

findPartNumbers::[String]->[Int]
findPartNumbers schematic = map (\(PartNumber _ _ value) -> value) (filter (isValidPartNumber schematic) (getAllNumbers schematic))

getAllNumbers::[String]->[Part]
getAllNumbers schematic = concatMap getAllNumbersOnLine (zip schematic [0..])

getAllNumbersOnLine::(String,Int)->[Part]
getAllNumbersOnLine (line, y) = getAllNumbersOnLine' y 0
  where
    getAllNumbersOnLine' y x = case findNextNumber (drop x line) y x of
                                    Nothing-> []
                                    Just (PartNumber _ foundX value) -> PartNumber y foundX value : getAllNumbersOnLine' y (foundX + getPowerOf10 value + 1)

findNextNumber::String->Int->Int->Maybe Part
findNextNumber [] y x = Nothing
findNextNumber line y x = if isDigit (head line) then Just (PartNumber y x (parseInt line)) else findNextNumber (tail line) y (x + 1)

get::[String]->Int->Int->Maybe Char
get schematic y x = if y >= 0 && y < height && x >= 0 && x < width then Just (schematic !! y !! x) else Nothing
  where
    height = length schematic
    width = length (head schematic)

parseInt::String->Int
parseInt str = read (takeWhile isDigit str)::Int

getPowerOf10::Int->Int
getPowerOf10 value = (floor . logBase 10 . fromInteger) (toInteger  value)
