{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Main where
import System.Environment
import Data.List.Extra
import Data.Char (isDigit)
import qualified Data.Set as Set

type Pos = (Int, Int)
type Robot = (Pos, Pos)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        computeRobotsPosition (map parseRobot (lines text)) 0
      _ -> putStrLn "Wrong number of arguments"


computeRobotsPosition::[Robot]->Int->IO()
computeRobotsPosition robots step  = do
    putStrLn $ "Second elapsed : " ++ show step
    printRobots $ Set.fromList (map fst robots)
    userInput <- getLine
    let newRobots =  map (computeNextXSecond 1) robots
    computeRobotsPosition newRobots (step + 1)

computeNextXSecond::Int->Robot->Robot
computeNextXSecond s ((startX, startY), (dirX, dirY)) = (((dirX * s  + startX) `mod` 101 , (dirY * s  + startY) `mod` 103), (dirX, dirY))


printRobots::Set.Set Pos->IO()
printRobots robots = mapM_ printTiles (zip [0..102] (repeat [0..100]))
  where
    printTiles (y, xs) = do
      mapM_ (\x -> printTile (x,y))  xs
      putStrLn ""
    printTile pos = putStr $ if Set.member pos robots then "#" else "."


parseRobot::String->Robot
parseRobot input =  let [start, dir] = splitOn " " input
                        [startX, startY] = (map read . splitOn "," . drop 2) start
                        [dirX, dirY] = (map read . splitOn "," . drop 2) dir
                    in ((startX, startY), (dirX, dirY))