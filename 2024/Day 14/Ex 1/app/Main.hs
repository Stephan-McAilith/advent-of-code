module Main where
import System.Environment
import Data.List.Extra
import Data.Char (isDigit)

type Vector = (Int, Int)
type Robot = (Vector, Vector)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ computeQuadrantSafety . computeRobotsPosition $ map parseRobot (lines text)
      _ -> putStrLn "Wrong number of arguments"

computeQuadrantSafety::[Vector]->Int
computeQuadrantSafety = product . map length . group . dropWhile (== 0) . sort . map findQuadrant
  where
    findQuadrant (x,y)
      | x < 50 && y < 51 = 1
      | x > 50 && y < 51 = 2
      | x < 50 && y > 51 = 3
      | x > 50 && y > 51 = 4
      | otherwise = 0

computeRobotsPosition::[Robot]->[Vector]
computeRobotsPosition = map computePosIn100Second
  where
    computePosIn100Second ((startX, startY), (dirX, dirY)) = ((100 * dirX  + startX) `mod` 101 , (100 * dirY  + startY) `mod` 103)
parseRobot::String->Robot
parseRobot input =  let [start, dir] = splitOn " " input
                        [startX, startY] = (map read . splitOn "," . drop 2) start
                        [dirX, dirY] = (map read . splitOn "," . drop 2) dir
                    in ((startX, startY), (dirX, dirY))