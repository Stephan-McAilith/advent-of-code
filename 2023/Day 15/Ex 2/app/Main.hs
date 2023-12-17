module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra
import Debug.Trace
import Text.Parsec
import Text.Parsec.String
import Data.Maybe


data Instruction = AddLens Int String Int
                  | RemoveLens Int String
                  deriving (Eq, Show)
type Lens = (String, Int)

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ sum $ zipWith computeBoxFocusingPower [1..] (computeLensBoxes text)
      _ -> putStrLn "Wrong number of arguments"

computeBoxFocusingPower::Int->[Lens]->Int
computeBoxFocusingPower boxIndex lenses = sum $ zipWith (\lensIndex lens -> snd lens * lensIndex * boxIndex) [1..] lenses

computeLensBoxes::String->[[Lens]]
computeLensBoxes text = foldl applyInstruction (replicate 256 []) $ map parseInstruction $ splitOn "," text

applyInstruction::[[Lens]]->Instruction->[[Lens]]
applyInstruction lenses (AddLens hash label focal)
  | null boxLenses = take hash lenses ++ [(label, focal)] : drop (hash + 1) lenses
  | otherwise = take hash lenses ++ addLens boxLenses hash label focal : drop (hash + 1) lenses
  where
    boxLenses = lenses !! hash
applyInstruction lenses (RemoveLens hash label)
  | null boxLenses = lenses
  | otherwise = take hash lenses ++ removeLens boxLenses hash label : drop (hash + 1) lenses
  where
    boxLenses = lenses !! hash

addLens::[Lens]->Int->String->Int->[Lens]
addLens boxLenses hash label focal = case lensLabelIndex of
                  Nothing -> boxLenses ++ [(label, focal)]
                  Just index -> take index boxLenses ++ (label, focal) : drop (index + 1) boxLenses
  where
    lensLabelIndex = findIndex ((== label) . fst) boxLenses

removeLens::[Lens]->Int->String->[Lens]
removeLens boxLenses hash label = case lensLabelIndex of
                  Nothing-> boxLenses
                  Just index -> take index boxLenses ++ drop (index + 1) boxLenses
  where
    lensLabelIndex = findIndex ((== label) . fst) boxLenses

parseInstruction::String->Instruction
parseInstruction input = case parse pInstruction "instruction" input of
                    Right value -> value
                    Left e -> error "Invalid Instruction"

pInstruction::Parser Instruction
pInstruction = do
  label <- many1 letter
  intructionType <- char '=' <|> char '-'
  case intructionType of
    '-' -> return (RemoveLens (computeHash label) label)
    '=' -> digit >>= \d -> return (AddLens (computeHash label) label (digitToInt d))

computeHash = foldl  (\ currentValue x -> ((currentValue + ord x) * 17) `mod` 256) 0