module Main where
import System.Environment
import Data.Maybe
import Data.List.Extra
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
                              in length $ evalState (filterM (findMatch tree) (lines towels)) S.empty

findMatch::Node->String->State (S.Set String) Bool
findMatch root testTowel = findMatch' root testTowel
  where
    findMatch'::Node->String->State (S.Set String) Bool
    findMatch' (Node isEnd _) [] = return isEnd
    findMatch' (Node _ nexts) towel@(t:tw) = case nexts M.!? t of
                                              Nothing -> return False
                                              Just node@(Node isEnd nodes) -> findMatch' node tw >>= \fail -> if fail then return True 
                                                                              else do
                                                                                failed <- gets (S.member tw)
                                                                                if failed || not isEnd then return False
                                                                                else findMatch' root tw >>= \fail-> if fail then return True
                                                                                      else modify (S.insert tw) >> return False


buildTree::[String]->Node
buildTree = foldl addPattern (Node False M.empty)
  where
    addPattern (Node _ next) []  = Node True next
    addPattern (Node isEnd next) (p:pattern) =  let node = M.findWithDefault (Node False M.empty) p next
                                                in Node isEnd (M.insert p (addPattern node pattern) next)