module Main where
import System.Environment
import Data.Char (isDigit)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.Bits (xor)
import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe

type StateIO s a  = StateT s IO a
type Registers = (Int, Int, Int)
type Program = [Int]

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        process (lines text)
        putStr "\b "
      _ -> putStrLn "Wrong number of arguments"

process::[String]->IO ()
process [a,b,c, _, p] = let
    registerA = (read . dropWhile (not . isDigit)) a
    registerB = (read . dropWhile (not . isDigit)) b
    registerC = (read . dropWhile (not . isDigit)) c
    program = (map read . splitOn "," . dropWhile (not . isDigit)) p
    in evalStateT (playProgram program) (registerA, registerB, registerC)

playProgram::Program->StateIO Registers ()
playProgram program = playProgram' 0
  where
    playProgram' index
      | index >= length program = return ()
      | otherwise = do
        newIndex <- executeInstruction index (program !! index) (program !! (index + 1))
        playProgram' newIndex

executeInstruction::Int->Int->Int->StateIO Registers Int
executeInstruction index 0 operand = combo operand >>= adv index
executeInstruction index 1 operand = bxl index operand
executeInstruction index 2 operand = combo operand >>= bst index
executeInstruction index 3 operand = jnz index operand
executeInstruction index 4 operand = bxc index
executeInstruction index 5 operand = combo operand >>= out index
executeInstruction index 6 operand = combo operand >>= bdv index
executeInstruction index 7 operand = combo operand >>= cdv index

combo::Int->StateIO Registers Int
combo 4 = gets fst3
combo 5 = gets snd3
combo 6 = gets thd3
combo operand = return operand

adv::Int->Int->StateIO Registers Int
adv index operand = do
  (a,b,c) <- get
  put (a `div` (2 ^ operand), b, c)
  return (index + 2)

bxl::Int->Int->StateIO Registers Int
bxl index operand = do
  (a,b,c) <- get
  put (a, b `xor` operand, c)
  return (index + 2)

bst::Int->Int->StateIO Registers Int
bst index operand = do
  (a,b,c) <- get
  put (a, operand `mod` 8, c)
  return (index + 2)

jnz::Int->Int->StateIO Registers Int
jnz index operand = do
  (a,b,c) <- get
  return (if a == 0 then index + 2 else operand)

bxc::Int->StateIO Registers Int
bxc index = do
  (a,b,c) <- get
  put (a, b `xor` c, c)
  return (index + 2)

out::Int->Int->StateIO Registers Int
out index operand = do
  liftIO $ putStr $ show (operand `mod` 8) ++ ","
  return (index + 2)

bdv::Int->Int->StateIO Registers Int
bdv index operand = do
  (a,b,c) <- get
  put (a, a `div` (2 ^ operand), c)
  return (index + 2)

cdv::Int->Int->StateIO Registers Int
cdv index operand = do
  (a,b,c) <- get
  put (a, b, a `div` (2 ^ operand))
  return (index + 2)
