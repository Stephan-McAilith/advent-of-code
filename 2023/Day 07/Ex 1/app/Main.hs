{-# LANGUAGE InstanceSigs #-}

module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Maybe

-- type goes from 1 to 7 depending in it strengh 1 beeing High card and 7 Five of a kind 
data Hand = Hand { hand::[Int], bid::Int, handType::Int } deriving (Show, Eq)

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1 h2 = compare (handType h1, hand h1) (handType h2, hand h2)

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ getTotalWining text
      _ -> putStrLn "Wrong number of arguments"


getTotalWining :: String -> Int
getTotalWining input = foldr ((+) . (\(hand, index) -> bid hand * index)) 0 $ zip hands [1..]
  where
    hands = sort $ map getHand $ lines input

getHand::String->Hand
getHand input = Hand cardsValues (read $ last splitedInput) (getHandType cardsValues)
  where
    cardsValues = mapMaybe (`elemIndex ` "23456789TJQKA") $ head splitedInput
    splitedInput = words input

getHandType::[Int]->Int
getHandType cards =  case groupedCards of
                      [5] -> 7
                      [1,4] -> 6
                      [2,3] -> 5
                      [1,1,3] -> 4
                      [1,2,2] -> 3
                      [1,1,1,2] -> 2
                      _ -> 1
  where
    groupedCards = sort $ map length $ group $ sort cards
