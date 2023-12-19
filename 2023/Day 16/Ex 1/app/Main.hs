module Main where
import System.Environment
import Data.Char
import Data.List
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Debug.Trace
import Data.Maybe

data Direction = Rightward | Leftward | Upward | Downward deriving (Eq, Show)

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ countEnergizedTiles $ lines text
      _ -> putStrLn "Wrong number of arguments"

countEnergizedTiles::[[Char]]->Int
countEnergizedTiles tiles = let (_ , resultMap) = runState (checkIfContinue tiles 0 0 Rightward) Map.empty in Map.size resultMap

raytracing::[[Char]]->Int->Int->Direction->State (Map.Map (Int, Int) [Direction]) ()
raytracing tiles y x direction = case currentTile of
                                  '.' -> case direction of
                                      Rightward -> checkIfContinue tiles y (x + 1) Rightward
                                      Leftward -> checkIfContinue tiles y (x - 1) Leftward
                                      Downward -> checkIfContinue tiles (y + 1) x Downward
                                      Upward -> checkIfContinue tiles (y - 1) x Upward
                                  '/' -> case direction of
                                      Rightward -> checkIfContinue tiles (y - 1) x Upward
                                      Leftward -> checkIfContinue tiles (y + 1) x Downward
                                      Downward -> checkIfContinue tiles y (x - 1) Leftward
                                      Upward -> checkIfContinue tiles y (x + 1) Rightward
                                  '\\' -> case direction of
                                      Rightward -> checkIfContinue tiles (y + 1) x Downward
                                      Leftward -> checkIfContinue tiles (y - 1) x Upward
                                      Downward -> checkIfContinue tiles y (x + 1) Rightward
                                      Upward -> checkIfContinue tiles y (x - 1) Leftward
                                  '|' -> case direction of
                                      Downward -> checkIfContinue tiles (y + 1) x Downward
                                      Upward -> checkIfContinue tiles (y - 1) x Upward
                                      _ -> checkIfContinue tiles (y - 1) x Upward >> checkIfContinue tiles (y + 1) x Downward
                                  '-' -> case direction of
                                      Rightward -> checkIfContinue tiles y (x + 1) Rightward
                                      Leftward -> checkIfContinue tiles y (x - 1) Leftward
                                      _ -> checkIfContinue tiles y (x + 1) Rightward >> checkIfContinue tiles y (x - 1) Leftward
  where
    currentTile = tiles !! y !! x

checkIfContinue::[[Char]]->Int->Int->Direction->State (Map.Map (Int, Int) [Direction]) ()
checkIfContinue tiles y x direction
  | x >= length tiles || x < 0  = pure ()
  | y >= length tiles || y < 0  = pure ()
  | otherwise = get >>= \seen-> case  Map.lookup  (x,y) seen of
                                  Nothing -> put (Map.insert (x,y) [direction] seen) >> raytracing tiles y x direction
                                  Just directions -> if direction `elem` directions then pure () else put (Map.insert (x,y) (direction:directions) seen) >> raytracing tiles y x direction


