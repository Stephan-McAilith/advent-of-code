module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Maybe
import Data.List.Extra

data Card = Scratchcard Int Int

instance Show Card where
    show (Scratchcard id goodNumbers) = "Scratchcard " ++ show id ++ " " ++ show goodNumbers ++ "\n"

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (length (getScratchcards text))
      _ -> putStrLn "Wrong number of arguments"

getScratchcards text = computeCopies (map getScratchcard (lines text))

getScratchcard :: String -> Card
getScratchcard str = Scratchcard id (length (filter (`elem` winningNumbers) myNumbers))
  where
    id = read (last (words (head (splitOn ":" str))))::Int
    numbers = splitOn "|" (last (splitOn ":" str))
    winningNumbers = parseInts (head numbers)
    myNumbers = parseInts (last numbers)

parseInts::String->[Int]
parseInts str = map read (words str)

computeCopies::[Card]->[Card]
computeCopies [] = [];
computeCopies ((Scratchcard id goodNumbers):cards) =  Scratchcard id goodNumbers : computeCopies (getCopies id goodNumbers cards ++ cards)

getCopies::Int->Int->[Card]->[Card]
getCopies baseId nbCopies cards = mapMaybe ((`find` cards) . matchCardById)  [baseId + 1 .. baseId + nbCopies]

matchCardById::Int -> (Card-> Bool)
matchCardById searchedId (Scratchcard id goodNumbers) = id == searchedId
