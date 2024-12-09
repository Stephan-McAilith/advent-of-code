module Main where
import System.Environment
import Data.List
import Data.Char (digitToInt)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State

type File = (Int,Int,Int)
type FreeSpace = (Int,Int)

type FreeSpaces = (Set.Set FreeSpace)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        let (files, freeSpaces) = parseDisk text
        print $ computeCheckSum (shift files freeSpaces)
      _ -> putStrLn "Wrong number of arguments"

shift::[File]->FreeSpaces->[File]
shift files freeSpaces = let (compressedfiles, _) = runState (mapM shiftFile files) freeSpaces in  compressedfiles 

computeCheckSum::[File]->Int
computeCheckSum = foldr ((+).(\(index, size, value)-> sum [value * (index + i) | i <- [0..(size-1)]])) 0

shiftFile::File->State FreeSpaces File
shiftFile (index, size, value) = do
  freeSpaces <- gets (Set.filter (\(index', size')-> index' < index && size' >= size))
  case Set.minView freeSpaces of
    Nothing -> return (index, size, value)
    Just (x, _) -> computeShit (index, size, value) x

computeShit::File->FreeSpace->State FreeSpaces File
computeShit (index, size, value) (index', size') = do
  let newFile = (index', size, value)
  let (newIndex, newSize) = (index' + size, size' - size)
  modify (Set.insert (newIndex, newSize) . Set.delete (index', size'))
  return newFile

parseDisk::String->([File], FreeSpaces)
parseDisk diskMap = parseDisk' (zip [0..] (map digitToInt diskMap)) [] Set.empty 0
  where
    parseDisk' [] files freeSpaces _ = (files, freeSpaces)
    parseDisk' ((value,size):disk) files freeSpaces index
      | even value = parseDisk' disk ((index, size, value `div` 2):files) freeSpaces (index + size)
      | otherwise = parseDisk' disk files (Set.insert (index, size) freeSpaces) (index + size)

