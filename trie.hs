import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: Map.Map Char Trie} deriving (Show, Eq)
type Word = String

empty :: Trie
empty = Trie {end = True, children = Map.empty}

insert :: Word -> Trie -> Trie
insert [] _ = empty
insert (x:xs) t
    | xs == []                               = Trie {end = True, children = Map.insert x (fromMaybe empty $ Map.lookup x (children t)) $ children t}
    | (Map.lookup x $ children t) == Nothing = Trie {end = False, children = Map.insert x (insert xs (fromMaybe empty $ Map.lookup x (children t))) (children t)} 
    | otherwise                              = Trie {end = (end t), children = Map.insert x (insert xs $ fromJust (Map.lookup x $ children t)) (children t)}

insertList :: [Word] -> Trie
insertList ws = foldr insert empty $ reverse ws

search :: Word -> Trie -> Bool
search [] _     = False
search (x:xs) t
    | xs == [] && (Map.lookup x $ children t) == Nothing  = False
    | xs == []                                            = if (end t) == True then True else False
    | (Map.lookup x $ children t) == Nothing              = False
    | otherwise                                           = search xs $ fromJust (Map.lookup x $ children t)

getWords :: Trie -> [Word]
getWords t = nub $ map reverse $ helper [] [] t
    where
        helper :: [Char] -> [Word] -> Trie -> [Word]
        helper cs ws t
            | t == empty      = (cs:ws)
            | (end t) == True = ws ++ concat (map (\(c,t') -> helper (c:cs) ((c:cs):ws) t') (Map.toList $ children t))
            | otherwise       = ws ++ concat (map (\(c,t') -> helper (c:cs) ws t') $ (Map.toList $ children t))

{-
prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined
-}
