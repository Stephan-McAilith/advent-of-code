import System.Environment
import Data.Char (isDigit, digitToInt)
import Data.List
import Distribution.Compat.CharParsing (digit)
import Data.Time.Clock.POSIX (getPOSIXTime)

digitNames = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "0", "1", "2","3","4","5","6","7","8","9"]

myDigitToInt "one" = 1
myDigitToInt "two" = 2
myDigitToInt "three" = 3
myDigitToInt "four" = 4
myDigitToInt "five" = 5
myDigitToInt "six" = 6
myDigitToInt "seven" = 7
myDigitToInt "eight" = 8
myDigitToInt "nine" = 9
myDigitToInt [x] = digitToInt x


main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print (foldr ((+) . processLine) 0  (lines text))
      _ -> putStrLn "Wrong number of arguments"

processLine :: [Char] -> Int
processLine str = getFirstDigit str* 10 +  getLastDigit str

getFirstDigit str = getFirstDigit' str []
  where
    getFirstDigit' str [] = getFirstDigit' (tail str) (dropWhile (\digit -> not (digit `isPrefixOf` str)) digitNames)
    getFirstDigit' str (digit:other) = myDigitToInt digit

getLastDigit str = getLastDigit' str []
  where
    getLastDigit' str [] = getLastDigit' (init str) (dropWhile (\digit -> not (digit `isSuffixOf` str )) digitNames)
    getLastDigit' str (digit:other) =  myDigitToInt digit



