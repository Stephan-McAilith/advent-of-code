module Main where
import System.Environment
import Data.Char
import Data.List
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Applicative

import Debug.Trace
import Data.Maybe

data Direction = Rightward | Leftward | Upward | Downward deriving (Eq, Show, Ord)


type SeenTiles = Set.Set (Int, Int)
type SeenPosition = Map.Map (Int, Int, Direction) SeenTiles

main::IO()
main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        let tiles = lines text
        let (maxEnergized, _) = foldr (\startPos (currentMaxEnergized, currentSeenPosition) ->
                    let (energized, newSeenPosition) = countEnergizedTiles tiles startPos Map.empty --currentSeenPosition | I have no idea why it does not works with currentSeenPosition
                    in (max energized currentMaxEnergized, newSeenPosition)) (0, Map.empty) (getListOfStartPosition tiles)
        print maxEnergized
      _ -> putStrLn "Wrong number of arguments"

getListOfStartPosition::[[Char]]->[(Int, Int, Direction)]
getListOfStartPosition tiles = let size = length tiles in concat [[(0,v,Downward), (size-1,v,Upward), (v, size-1, Leftward),  (v, 0, Rightward)] | v <- [0..(size-1)]]


countEnergizedTiles::[[Char]]->(Int, Int, Direction)->SeenPosition->(Int, SeenPosition)
countEnergizedTiles tiles tmp@(starty, startx, startDirection) startingState = let (energized, seenPosition) = runState (checkIfContinue tiles starty startx startDirection) startingState in (Set.size energized, seenPosition)

raytracing::[[Char]]->Int->Int->Direction->State SeenPosition SeenTiles
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
                                      _ -> liftA2 Set.union (checkIfContinue tiles (y - 1) x Upward) (checkIfContinue tiles (y + 1) x Downward)
                                  '-' -> case direction of
                                      Rightward -> checkIfContinue tiles y (x + 1) Rightward
                                      Leftward -> checkIfContinue tiles y (x - 1) Leftward
                                      _ ->  liftA2 Set.union (checkIfContinue tiles y (x + 1) Rightward) (checkIfContinue tiles y (x - 1) Leftward)
  where
    currentTile = tiles !! y !! x

checkIfContinue::[[Char]]->Int->Int->Direction->State SeenPosition SeenTiles
checkIfContinue tiles y x direction
  | x >= length tiles || x < 0  = pure Set.empty
  | y >= length tiles || y < 0  = pure Set.empty
  | otherwise = get >>= \seen-> case  Map.lookup (x,y, direction) seen of
                                  Nothing -> put (Map.insert (x, y, direction) Set.empty seen)
                                        >> raytracing tiles y x direction
                                        >>= \seenTiles -> let updatedSeen = Set.insert (x,y) seenTiles
                                        in get >>= \newSeen -> put (Map.insert (x,y, direction) updatedSeen newSeen)
                                        >> pure updatedSeen
                                  Just seenTiles -> pure seenTiles



