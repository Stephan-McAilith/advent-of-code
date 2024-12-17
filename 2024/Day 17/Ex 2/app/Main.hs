module Main where
import System.Environment
import Data.Char (isDigit)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.Bits (xor)
import Data.List.Extra
import Data.Tuple.Extra
import Debug.Trace


type Registers = (Int, Int, Int)
type Program = [Int]

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ hack (lines text)
      _ -> putStrLn "Wrong number of arguments"

hack::[String]->Int
hack [a,b,c, _, p] = let
    program = (map read . splitOn "," . dropWhile (not . isDigit)) p
    in hackProgram program

hackProgram::Program->Int
hackProgram program = hackSteps program (reverse program) 0

hackSteps::Program->[Int]->Int->Int
hackSteps _ [] current = current
hackSteps program (targetOut:rem) current = (foldl min (maxBound::Int) . map (hackSteps program rem) . filter tryHack) [current * 8 + x | x <- [0..7]]
  where
    tryHack testA = evalState (playProgram program) (testA, 0, 0) == targetOut

playProgram::Program->State Registers Int
playProgram program = foldM (\output (opcode, operand) -> executeInstruction output opcode operand) (-1) (pairUp program)

executeInstruction::Int->Int->Int->State Registers Int
executeInstruction output 0 operand = combo operand >>= adv >> return output
executeInstruction output 1 operand = bxl operand >> return output
executeInstruction output 2 operand = combo operand >>= bst >> return output
executeInstruction output 3 operand = return output
executeInstruction output 4 operand = bxc >> return output
executeInstruction output 5 operand = combo operand >>= out
executeInstruction output 6 operand = combo operand >>= bdv >> return output
executeInstruction output 7 operand = combo operand >>= cdv >> return output

combo::Int->State Registers Int
combo 4 = gets fst3
combo 5 = gets snd3
combo 6 = gets thd3
combo operand = return operand

adv::Int->State Registers ()
adv operand = do
  (a,b,c) <- get
  put (a `div` (2 ^ operand), b, c)

bxl::Int->State Registers ()
bxl operand = do
  (a,b,c) <- get
  put (a, b `xor` operand, c)

bst::Int->State Registers ()
bst operand = do
  (a,b,c) <- get
  put (a, operand `mod` 8, c)

bxc::State Registers ()
bxc = do
  (a,b,c) <- get
  put (a, b `xor` c, c)

out::Int->State Registers Int
out operand = return (operand `mod` 8)

bdv::Int->State Registers ()
bdv operand = do
  (a,b,c) <- get
  put (a, a `div` (2 ^ operand), c)

cdv::Int->State Registers ()
cdv operand = do
  (a,b,c) <- get
  put (a, b, a `div` (2 ^ operand))


pairUp :: [Int] -> [(Int, Int)]
pairUp [] = []
pairUp (x:y:xs) = (x, y) : pairUp xs
pairUp _ = []

debug aled = trace (show aled) aled