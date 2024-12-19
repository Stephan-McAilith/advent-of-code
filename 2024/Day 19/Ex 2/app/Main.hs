module Main where
import System.Environment
import Data.Maybe
import Data.List.Extra
import Control.Monad.State
import qualified Data.Map.Strict as M

data Node = Node Bool (M.Map Char Node) deriving (Show)

main = do
    args <- getArgs
    case args of
      [file] -> do
        text <- readFile file
        print $ countPossiblePattern text
      _ -> putStrLn "Wrong number of arguments"


countPossiblePattern::String->Int
countPossiblePattern input =  let [patterns, towels] = splitOn "\n\n" input
                                  tree = buildTree (splitOn ", " patterns)
                              in sum $ evalState (mapM (findMatch tree) (lines towels)) M.empty

findMatch::Node->String->State (M.Map String Int) Int
findMatch root testTowel = findMatch' root testTowel
  where
    findMatch'::Node->String->State (M.Map String Int) Int
    findMatch' (Node isEnd _) [] = return (if isEnd then 1 else 0)
    findMatch' (Node _ nexts) towel@(t:tw) = case nexts M.!? t of
                                              Nothing -> return 0
                                              Just node@(Node isEnd nodes) -> do
                                                count1 <- findMatch' node tw
                                                if not isEnd then return count1
                                                else do
                                                  seen <- gets (M.!? tw)
                                                  case seen of
                                                    Just count2 -> return (count1 + count2)
                                                    Nothing -> findMatch' root tw >>= \count2 -> modify (M.insert tw count2) >> return (count1 + count2)


buildTree::[String]->Node
buildTree = foldl addPattern (Node False M.empty)
  where
    addPattern (Node _ next) []  = Node True next
    addPattern (Node isEnd next) (p:pattern) =  let node = M.findWithDefault (Node False M.empty) p next
                                                in Node isEnd (M.insert p (addPattern node pattern) next)