import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: Map.Map Char Trie} deriving (Show, Eq)
type Word = String

empty :: Trie
empty = Trie {end = False, children = Map.empty}

insert :: Word -> Trie -> Trie
insert [] _ = empty
insert (x:xs) t
    | xs == []                               = Trie {end = (end t), children = Map.insert x (Trie {end=True, children = children (fromMaybe empty $ Map.lookup x (children t))}) (children t)}
    | (Map.lookup x $ children t) == Nothing = Trie {end = (end t), children = Map.insert x (insert xs (fromMaybe empty $ Map.lookup x (children t))) (children t)}
    | otherwise                              = Trie {end = (end t), children = Map.insert x (insert xs $ fromJust (Map.lookup x $ children t)) (children t)}

insertList :: [Word] -> Trie
insertList ws = foldr insert empty $ reverse ws

search :: Word -> Trie -> Bool
search [] t     = if (end t) == True then True else False
search (x:xs) t
    | (Map.lookup x $ children t) == Nothing              = False
    | otherwise                                           = search xs $ fromJust (Map.lookup x $ children t)

getWords :: Trie -> [Word]
getWords t = nub $ map reverse $ helper [] [] t
    where
        helper :: [Char] -> [Word] -> Trie -> [Word]
        helper cs ws t
            | (children t) == Map.empty = ws ++ [cs]
            | (end t) == True           = concat (map (\(c,t') -> helper (c:cs) (ws++[cs]) t') (Map.toList $ children t))
            | otherwise                 = concat (map (\(c,t') -> helper (c:cs) ws t') $ (Map.toList $ children t))

prefix :: Word -> Trie -> Maybe [Word]
prefix w t = (prefix' w t)
    where
        prefix' :: Word -> Trie -> Maybe [Word]
        prefix' [] t' = Just $ map ((++) w) (getWords t')
        prefix' (x:xs) t'
            | (Map.lookup x $ children t') == Nothing = Nothing
            | (end t') == True && xs == []            = Just (fromJust (prefix' xs $ fromJust (Map.lookup x $ children t')))
            | otherwise                               = prefix' xs $ fromJust (Map.lookup x $ children t')
