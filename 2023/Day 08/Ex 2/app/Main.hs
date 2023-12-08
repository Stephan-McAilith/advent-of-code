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

goThroughNodes::Map.Map String (String, String) -> [Char] -> Int -> String ->  Int
goThroughNodes _ _ steps (_:_:"Z")  = steps
goThroughNodes nodes (dir:directions) steps currentPos = goThroughNodes nodes directions (steps + 1) (if dir == 'L' then left else right)
  where
    (left, right) = nodes Map.! currentPos

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        -- lcm compute the least common multiple
        print $ foldl lcm 1 $ countSteps text
      _ -> putStrLn "Wrong number of arguments"

countSteps::String-> [Int]
countSteps input = map (goThroughNodes (Map.fromList nodes) (cycle directions) 0) startingPositions
  where
    startingPositions = filter (isSuffixOf "A") $ map (\(key,(_,_)) -> key) nodes
    nodes = map getNode (lines $ last splitInput)
    directions = head splitInput
    splitInput = splitOn "\n\n" input


getNode::String->(String, (String, String))
getNode line = case parse nodeParser "nodes" line of
                Right value -> value
                Left e -> error "Invalid Node"

nodeParser::Parser (String, (String, String))
nodeParser = do
  key <- many1 (letter <|> digit)
  spaces
  _ <- char '=' >> spaces
  _ <- char '(' >> spaces
  left <- many1 (letter <|> digit)
  _ <- char ',' >> spaces
  right <- many1 (letter <|> digit)
  _ <- char ')' >> spaces
  return (key, (left, right))