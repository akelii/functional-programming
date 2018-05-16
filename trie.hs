import qualified Data.Map.Strict as Map
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: Map.Map Char Trie} deriving (Show, Eq)
type Word = String

empty :: Trie
empty = Trie {end = True, children = Map.empty}

insert :: Word -> Trie -> Trie
insert (x:xs) t
    | xs == []                                             = Trie {end = True, children = Map.insert x empty (children t)} 
    | fromMaybe empty (Map.lookup x $ children t) == empty = Trie {end = False, children = Map.insert x (insert xs (fromMaybe empty $ Map.lookup x (children t))) (children t)} 
    | otherwise                                            = insert xs $ fromJust (Map.lookup x $ children t)

insertList :: [Word] -> Trie
insertList ws = foldr insert empty ws

{-
search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined
-}
