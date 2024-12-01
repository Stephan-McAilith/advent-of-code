module Main where
import System.Environment
import Data.Maybe
import Data.List.Extra
import Control.Monad.State
import qualified Data.Map.Strict as Map

type LocationMap = Map.Map Int Int

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ processLines (lines text)
      _ -> putStrLn "Wrong number of arguments"

processLines::[String]->Int
processLines lines = let (leftList, locations) = runState (mapM parseLine lines) Map.empty
  in foldr ((+) . computeSimilarityScore locations) 0 leftList

computeSimilarityScore::LocationMap->Int->Int
computeSimilarityScore locations value = value * fromMaybe 0 (Map.lookup value locations)

parseLine::String-> State LocationMap Int
parseLine line = let [left, right] = splitOn "   " line; lValue = read left; rValue = read right
  in get >>= \locations -> put (Map.insertWith (+) rValue 1 locations) >> return lValue

