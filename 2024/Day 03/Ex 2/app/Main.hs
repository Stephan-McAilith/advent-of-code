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
process input = case runParser parseMuls True "" input of
                Right muls -> foldr ((+) . computeValue) 0 muls
                Left e -> error "Should not happen in AoC"

computeValue::Result->Int
computeValue (Mul x y) = x * y
computeValue Noop = 0

parseMuls::Parsec String Bool [Result]
parseMuls = many $ try parseMul <|> try parseDoOrNot <|> (anyChar >> return Noop)
parseMul = do
  string "mul("
  x <- parseNumber
  char ','
  y <- parseNumber
  char ')'
  s <- getState
  return $ if s then  Mul (read x::Int) (read y::Int) else Noop

parseNumber = try (count 3 digit)
               <|> try (count 2 digit)
               <|> count 1 digit

parseDoOrNot = try (string "don't()" >> putState False >> return Noop) <|> (string "do()" >> putState True >> return Noop)