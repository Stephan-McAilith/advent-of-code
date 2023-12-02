module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra



data Result d = Parsed d String

data Draw = Drawed Int String

isValidDraw::Draw->Bool
isValidDraw (Drawed nb "red") = nb <= 12
isValidDraw (Drawed nb "green") = nb <= 13
isValidDraw (Drawed nb "blue") = nb <= 14

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (foldr ((+) . processLine) 0  (lines text))
      _ -> putStrLn "Wrong number of arguments"


processLine :: String ->  Int
processLine str = if checkGame rem then id else 0
  where
    Parsed id rem = parseId str;

parseId::String -> Result Int
parseId str = Parsed id (tail(dropWhile (/= ':') rem))
  where
    Parsed id rem = parseInt trimed
    trimed = dropWhile (not . isDigit) str

checkGame::String->Bool;
checkGame str = foldr ((&&) . checkDraw . splitOn ",") True (splitOn ";" str)

checkDraw::[String]-> Bool
checkDraw = foldr ((&&) . isValidDraw . parseDraw) True


parseDraw::String->Draw
parseDraw str = Drawed nb color
  where
    color = trim rem
    Parsed nb rem = parseInt (trim str)

parseInt::String->Result Int
parseInt str = Parsed (read (takeWhile isDigit str)::Int) (dropWhile isDigit str)

