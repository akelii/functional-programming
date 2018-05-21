import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie   = Trie {end :: Bool, children :: Map.Map Char Trie} deriving (Show, Eq)
data Action = Add | Search | Find | Print | Exit | NoAction deriving (Show, Eq)
type Word   = String

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

convertAction :: Char -> Action
convertAction c
    | c `elem` "aA" = Add
    | c `elem` "sS" = Search
    | c `elem` "fF" = Find
    | c `elem` "pP" = Print
    | c `elem` "eE" = Exit
    | otherwise     = NoAction

getInput :: IO (Action, Word)
getInput = do 
    putStrLn "Enter the action:"
    c <- getLine
    let act = convertAction $ c!!0
    if (act == Print) || (act == Exit) || (act == NoAction) 
        then return (act, "")
        else do putStrLn "Enter word/prefix:"
                w <- getLine
                return (convertAction $ c!!0, w)

doAction :: Trie -> IO ()
doAction t = do
    (a,w) <- getInput
    if a == Find
        then do if (fromMaybe [] $ prefix w t) == [] then putStrLn "No words found with that prefix!" else putStrLn ("Found words:\n" ++ (unlines $ fromMaybe [] $ prefix w t))
                doAction t
        else if a == Add
            then do putStrLn "New word is added!"
                    doAction (insert w t)
            else if a == Search
                then do if (search w t) == True then putStrLn "Exists in dictionary!" else putStrLn "NOT exist!"
                        doAction t
                else if a == Print
                    then do putStrLn "List of words in dictionary:"
                            let result = unlines $ getWords t
                            putStrLn result
                            doAction t
                    else if a == Exit
                        then do putStrLn "Good bye dear user!"
                                return ()
                        else do putStrLn "Invalid action!"
                                menu <- getMenu
                                putStrLn menu
                                doAction t

getMenu :: IO String
getMenu = return "a) Add Word\ns) Search Word\nf) Find words with prefix\np) Print all words\ne) Exit"
