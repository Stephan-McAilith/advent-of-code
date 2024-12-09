module Main where
import System.Environment
import Data.List
import Data.Char (digitToInt)

data DiskSpace = File Int | FreeSpace deriving (Show, Eq)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ computeChecksum (compact text)
      _ -> putStrLn "Wrong number of arguments"

computeChecksum::[DiskSpace]->Int
computeChecksum diskSpace = let zipped = zip [0..] diskSpace in  computeChecksum' zipped (reverse (filter (\(_,slot) -> slot /= FreeSpace) zipped)) 0
  where
    computeChecksum _ [] _= 0
    computeChecksum [] _ _= 0
    computeChecksum' ((i,d):disk) ((ri,rd):rdisk) fileIndex
      | i > ri = 0
      | otherwise = case d of
          File v -> fileIndex * v + computeChecksum' disk ((ri,rd):rdisk) (fileIndex+1)
          FreeSpace ->  let File v = rd
                        in fileIndex * v + computeChecksum' disk rdisk (fileIndex+1)


compact::String->[DiskSpace]
compact diskMap = concatMap (\(i,v)-> replicate v (if even i then File (i `div` 2) else FreeSpace)) (zip [0..] (map digitToInt diskMap))

-- shiftSpace::[DiskSpace]->[DiskSpace]
-- shiftSpace [] = []
-- shiftSpace [d] = case d of
--                   File _ -> [d]
--                   FreeSpace -> []
-- shiftSpace (d:disk) = case d of
--                         File _ -> d : shiftSpace disk
--                         FreeSpace -> shiftSpace  $ last disk : init disk