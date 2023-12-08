module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra
import Text.Parsec
import Debug.Trace
import Text.Parsec.String
import qualified Data.Map.Strict as Map
import Data.Maybe

goThroughNodes::String -> Map.Map String (String, String) -> [Char] -> Int -> Int
goThroughNodes "ZZZ" _ _ steps = steps
goThroughNodes currentPos nodes (dir:directions) steps = goThroughNodes (if dir == 'L' then left else right) nodes directions $ steps + 1
  where
    (left, right) = nodes Map.! currentPos

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ countSteps text
      _ -> putStrLn "Wrong number of arguments"


countSteps::String-> Int
countSteps input = goThroughNodes "AAA" nodes (cycle directions) 0
  where
    nodes = Map.fromList $ map getNode (lines $ last splitInput)
    directions = head splitInput
    splitInput = splitOn "\n\n" input


getNode::String->(String, (String, String))
getNode line = case parse nodeParser "nodes" line of
                Right value -> value
                Left e -> error "Invalid Node"  

nodeParser::Parser (String, (String, String))
nodeParser = do
  key <- many1 letter
  spaces
  _ <- char '=' >> spaces
  _ <- char '(' >> spaces
  left <- many1 letter
  _ <- char ',' >> spaces
  right <- many1 letter
  _ <- char ')' >> spaces
  return (key, (left, right))