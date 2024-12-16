module Main where
import System.Environment
import Data.List.Extra
import Data.Char (isDigit)
import Control.Monad.State
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ((!), (//))

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
    rawPlan = lines left
    [left, right] = splitOn "\n\n" input


computeGPSCoordinates::Plan->Int
computeGPSCoordinates plan =sum $ findBoxesGPSCoordinate (map V.toList (V.toList plan))
  where
    findBoxesGPSCoordinate listPlan = [y * 100 + x | (row, y) <- zip listPlan [0..], (tile, x)<- zip row [0..], tile == 'O']

computeRobotMove::Plan->Char->State Pos Plan
computeRobotMove plan move
  | move == '^' = moveRobot (0,-1) plan
  | move == '>' = moveRobot (1,0) plan
  | move == 'v' = moveRobot (0,1) plan
  | move == '<' = moveRobot (-1,0) plan

moveRobot::(Int,Int)->Plan->State Pos Plan
moveRobot (dirX,dirY) plan = do
  (x,y) <- get
  case findAvailableTile (x,y) of
    Nothing -> return plan
    Just (freeX, freeY) -> put (x + dirX, y + dirY) >> return (updatePlan (freeX, freeY) (x,y) plan)
  where
    findAvailableTile::Pos->Maybe Pos
    findAvailableTile (posX,posY) = findAvailableTile'(posX + dirX, posY + dirY)
    findAvailableTile' (posX,posY) = case plan ! posY ! posX of
                                      '#' -> Nothing
                                      '.' -> Just (posX,posY)
                                      'O' -> findAvailableTile' (posX + dirX, posY + dirY)
    updatePlan::Pos->Pos->Plan->Plan
    updatePlan (destX, destY) (posX, posY) plan
      | destX == posX && destY == posY = plan // [(destY, (plan ! destY) // [(destX, '.')])]
      | otherwise = let copiedValue =  plan ! (destY - dirY) ! (destX - dirX)
                        prevPos = (destX - dirX, destY - dirY)
                    in  updatePlan prevPos (posX, posY) (plan // [(destY, (plan ! destY) // [(destX, copiedValue)])])


findRobot::[[Char]]->Pos
findRobot input = findRobot' input 0
  where
    findRobot' (r:rows) y = case elemIndex '@' r of
                        Just x -> (x, y)
                        Nothing -> findRobot' rows (y+1)