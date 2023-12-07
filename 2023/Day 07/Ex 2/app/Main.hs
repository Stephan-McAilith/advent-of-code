{-# LANGUAGE InstanceSigs #-}

module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord

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
        print $ getTotalWinning text
      _ -> putStrLn "Wrong number of arguments"


getTotalWinning :: String -> Int
getTotalWinning input = foldr ((+) . (\(hand, index) -> bid hand * index)) 0 $ zip hands [1..]
  where
    hands = sort $ map getHand $ lines input

getHand::String->Hand
getHand input = Hand cardsValues (read $ last splitedInput) (getHandType cardsValues)
  where
    cardsValues = mapMaybe (`elemIndex ` "J23456789TQKA") $ head splitedInput
    splitedInput = words input

getHandType::[Int]->Int
getHandType cards =  case groupedCardsWithJokers of
                      [5] -> 7
                      [4,1] -> 6
                      [3,2] -> 5
                      [3,1,1] -> 4
                      [2,2,1] -> 3
                      [2,1,1,1] -> 2
                      _ -> 1
  where
    groupedCardsWithJokers = case groupedCards of
      []->[jokerNumber]
      (firstGroup:otherGrous) -> firstGroup + jokerNumber : otherGrous
    jokerNumber = length $ filter (== 0) cards
    cardsWithoutJokers = filter (/= 0) cards
    groupedCards = sortBy (comparing Down) (map length $ group $ sort cardsWithoutJokers)
