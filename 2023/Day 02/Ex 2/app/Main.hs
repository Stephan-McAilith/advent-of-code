module Main where
import System.Environment
import Data.Char
import Data.List
import Data.List.Extra

data Result d = Parsed d String

data Draw = Drawed Int String
          | None
emptySet =  [Drawed 0 "red", Drawed 0 "green", Drawed 0 "blue"]

instance Show Draw where
    show (Drawed x str) = "Drawed " ++ show x ++ " " ++ show str

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (foldr ((+) . computeSetPower . computeMinSet) 0  (lines text))
      _ -> putStrLn "Wrong number of arguments"


computeSetPower::[Draw]->Int
computeSetPower draw = product (map (\(Drawed nb color) -> if nb /= 0 then nb else 1) draw)

computeMinSet::String -> [Draw]
computeMinSet str = foldr (mergeSet . parseSet) emptySet (splitOn ";" trimedStr)
  where
    _:(trimedStr:_) = splitOn ":" str

mergeSet::[Draw]->[Draw]->[Draw]
mergeSet l r = map (\(Drawed nb color) -> mergeDraw (Drawed nb color) (findDrawByColor color l)) r

mergeDraw::Draw->Draw->Draw
mergeDraw ldraw None = ldraw
mergeDraw (Drawed lnb color) (Drawed rnb _) = Drawed (max lnb rnb) color

findDrawByColor::String->[Draw]->Draw
findDrawByColor searchedColor draws = case filter (\(Drawed _ color) -> color == searchedColor) draws of
  [found] -> found
  [] -> None

parseSet::String-> [Draw]
parseSet str = map parseDraw (splitOn "," str)

parseDraw::String->Draw
parseDraw str = Drawed nb color
  where
    Parsed nb rem = parseInt (trim str)
    color = trim rem

parseInt::String->Result Int
parseInt str = Parsed (read digits::Int) (drop (length digits) str)
  where
    digits = takeWhile isDigit str

