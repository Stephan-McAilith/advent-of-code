module Main where
import System.Environment
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Debug.Trace

type RulesMap = Map.Map Int [Int]

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        let [rules, updates] = splitOn "\n\n" text
        print $ process text
      _ -> putStrLn "Wrong number of arguments"

process::String->Int
process input = let [rules, updates] = splitOn "\n\n" input
                in foldr ((+) . checkValidity (parseRules rules) ) 0 (parseUpdates updates)

checkValidity::RulesMap->[Int]->Int
checkValidity rules update = if checkValidity' update [] then update !! (length update `div` 2) else 0
  where
    checkValidity' [] _ = True
    checkValidity' (x:xs) banList
      | x `elem` banList = False
      | otherwise = checkValidity' xs $ banList ++ Map.findWithDefault [] x rules

parseUpdates::String->[[Int]]
parseUpdates input = map ((map read) . (splitOn ",")) (lines input)

parseRules::String->RulesMap
parseRules input = let rules = map parseRule (lines input) in foldr concatRule Map.empty rules

parseRule::String->(Int, Int)
parseRule rule = let (left, (_:right)) = break (== '|')  rule in (read left, read right)

concatRule::(Int, Int)->RulesMap->RulesMap
concatRule (before, after) rules = Map.insertWith (++) after [before] rules

debug aled = trace (show aled) aled