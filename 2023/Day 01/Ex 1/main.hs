import System.Environment
import Data.Char (isDigit, digitToInt)

main = do
    args <- getArgs
    case args of 
      [file] -> do
        text <- readFile file
        print (foldr ((+) . processLine) 0  (lines text))
      _ -> putStrLn "Wrong number of arguments"

processLine str = combineFirstAndLast (filter isDigit str)
combineFirstAndLast str = digitToInt (head str) * 10 + digitToInt (last str)

