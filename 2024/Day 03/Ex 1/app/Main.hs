module Main where
import System.Environment
import Text.Parsec
import Text.Parsec.String

data Result = Mul Int Int
            | Noop

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ process text
      _ -> putStrLn "Wrong number of arguments"

process::String->Int
process input = case parse parseMuls "" input of
                Right muls -> foldr ((+) . computeValue) 0 muls
                Left e -> error "Should not happen in AoC"

computeValue::Result->Int
computeValue (Mul x y) = x * y
computeValue Noop = 0

parseMuls::Parser [Result]
parseMuls = many $ try parseMul <|> (anyChar >> return Noop)
parseMul = do
  string "mul("
  x <- parseNumber
  char ','
  y <- parseNumber
  char ')'
  return $ Mul (read x::Int) (read y::Int)

parseNumber = try (count 3 digit)
               <|> try (count 2 digit)
               <|> count 1 digit