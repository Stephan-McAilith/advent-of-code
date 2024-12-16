module Main where
import System.Environment
import Data.List.Extra
import Data.Char (isDigit)
import Control.Monad.State
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ((!), (//))
import qualified Data.Set as S

type Pos = (Int, Int)
type Plan = V.Vector (V.Vector Char)


main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ (computeGPSCoordinates . computeRobotMoves) text
      _ -> putStrLn "Wrong number of arguments"


computeRobotMoves::String->Plan
computeRobotMoves input = let (updatedPlan, robotPos) = runState (foldM computeRobotMove plan moves) (findRobot rawPlan) in updatedPlan
  where
    moves = concat (lines right)
    plan = V.fromList $  map V.fromList rawPlan
    rawPlan = widenPlan $ lines left
    [left, right] = splitOn "\n\n" input


computeGPSCoordinates::Plan->Int
computeGPSCoordinates plan = sum $ findBoxesGPSCoordinate (map V.toList (V.toList plan))
  where
    findBoxesGPSCoordinate listPlan = [y * 100 + x | (row, y) <- zip listPlan [0..], (tile, x)<- zip row [0..], tile == '[']

computeRobotMove::Plan->Char->State Pos Plan
computeRobotMove plan move
  | move == '^' = moveRobot (0,-1) plan
  | move == '>' = moveRobot (1,0) plan
  | move == 'v' = moveRobot (0,1) plan
  | move == '<' = moveRobot (-1,0) plan


moveRobot::(Int,Int)->Plan->State Pos Plan
moveRobot (dirX,dirY) plan = do
  (x,y) <- get
  case findMovingTiles (x,y) S.empty of
    Nothing -> return plan
    Just movedTiles -> put (x + dirX, y + dirY) >> return (updatePlan movedTiles (x,y))
    where
      updatePlan::S.Set Pos->Pos->Plan
      updatePlan movedTiles (x,y)=  let updatedPlan = S.foldr moveTile plan movedTiles
                                    in updatedPlan // [(y, (updatedPlan ! y) // [(x, '.')])]
        where
          moveTile (tx,ty) updatedPlan
            | not $ S.member (tx - dirX,ty - dirY) movedTiles = let tileMovedPlan = updatedPlan // [(ty + dirY, (updatedPlan ! (ty + dirY)) // [(tx + dirX, plan ! ty ! tx)])]
                                                                in tileMovedPlan // [(ty, (tileMovedPlan ! ty) // [(tx, '.')])]
            | otherwise = updatedPlan // [(ty + dirY, (updatedPlan ! (ty + dirY)) // [(tx + dirX, plan ! ty ! tx)])]

      findMovingTiles::Pos->S.Set Pos->Maybe (S.Set Pos)
      findMovingTiles (x,y) visited
        | S.member (x,y) visited  = Just S.empty
        | otherwise = case  plan ! (y + dirY) ! (x + dirX) of
                      '#'-> Nothing
                      '.'-> Just $ S.singleton (x,y)
                      '['-> let left = findMovingTiles (x + dirX, y + dirY) updatedVisited
                                right = findMovingTiles (x + 1 + dirX, y + dirY) updatedVisited
                            in if isJust left && isJust right then Just  (S.insert (x,y) (S.union (fromJust left) (fromJust right))) else Nothing
                      ']'-> let right = findMovingTiles (x + dirX, y + dirY) updatedVisited
                                left = findMovingTiles (x - 1 + dirX, y + dirY) updatedVisited
                            in if isJust left && isJust right then Just (S.insert (x,y) (S.union (fromJust left) (fromJust right))) else Nothing
        where
          updatedVisited = S.insert (x,y) visited

findRobot::[[Char]]->Pos
findRobot input = findRobot' input 0
  where
    findRobot' (r:rows) y = case elemIndex '@' r of
                        Just x -> (x, y)
                        Nothing -> findRobot' rows (y+1)


widenPlan::[[Char]]->[[Char]]
widenPlan = map (concatMap widen)
  where
    widen '#' = "##"
    widen '@' = "@."
    widen 'O' = "[]"
    widen '.' = ".."
