import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie   = Trie {end :: Bool, children :: Map.Map Char Trie} deriving (Show, Eq)
data Action = Add | Search | Find | Print | Exit | NoAction deriving (Show, Eq)
type Word   = String

--DESC: creates a new empty trie.
empty :: Trie
empty = Trie {end = False, children = Map.empty}


{-
DESC:          takes a word and a trie; then the given word is inserted into the given trie and new trie is returned.
HOW ITs WORK?: insert function recursively appends the child element with holding the starting point (parent node)
If the inserted word is already in the Trie then again try to insert with keeping the parent node.
Remember that there is only one element in the Map with the same key so re-inserting is not affect the Trie.
-}
insert :: Word -> Trie -> Trie
insert [] _ = empty
insert (x:xs) t
    | xs == []                               = Trie {end = (end t), children = Map.insert x (Trie {end=True, children = children (fromMaybe empty $ Map.lookup x (children t))}) (children t)}
    | (Map.lookup x $ children t) == Nothing = Trie {end = (end t), children = Map.insert x (insert xs (fromMaybe empty $ Map.lookup x (children t))) (children t)}
    | otherwise                              = Trie {end = (end t), children = Map.insert x (insert xs $ fromJust (Map.lookup x $ children t)) (children t)}

{-
DESC:          takes a list of words and return a trie which contains the words given in the parameter
HOW ITs WORK?: foldr start with at the end of the list so reversing the list makes sence to keep inserting order
-}
insertList :: [Word] -> Trie
insertList ws = foldr insert empty $ reverse ws

{-
DESC:          takes a word and a trie; it returns true if the given word is in the trie, false otherwise.
HOW ITs WORK?: search function compares whole word char by char until the end of the word. In each time, it looks next level in 
the Trie. If any key (char) in the Trie is not equal the current char then it directly return the False.  
-}
search :: Word -> Trie -> Bool
search [] t     = if (end t) == True then True else False
search (x:xs) t
    | (Map.lookup x $ children t) == Nothing              = False
    | otherwise                                           = search xs $ fromJust (Map.lookup x $ children t)

{-
DESC:          takes a trie and returns all the words in the trie.
HOW ITs WORK?: helper function (getWords') takes [Char] to keep where it came from and 
                                           takes [Word] to keep the words as accumulator and
										   the Trie to pass the current level in Trie to the next recursion.
After then some words are in double because there can be multiple words passing from the same meaningful word.
For example "anne" and "aniden" pass the word "an" (which is also meaningful). So the nub function is necessary.
-}
getWords :: Trie -> [Word]
getWords t = nub $ map reverse $ getWords' [] [] t
    where
        getWords' :: [Char] -> [Word] -> Trie -> [Word]
        getWords' cs ws t
            | (children t) == Map.empty = ws ++ [cs]
            | (end t) == True           = concat $ map (\(c,t') -> getWords' (c:cs) (ws++[cs]) t') (Map.toList $ children t)
            | otherwise                 = concat $ map (\(c,t') -> getWords' (c:cs) ws t') (Map.toList $ children t)

{-
DESC:          takes a string (prefix) and a trie; then return whole words begins with given prefix.
HOW ITs WORK?: given prefix is recursively is furthered through the Trie. If any character in the prefix 
coudn't found in the current level of the Trie then directly returns the Nothing. Otherwise it progress
until the at the end of the word. And it is called getWords for the Trie of the last character. 
Then prefix is added the result of the getWords function.
-}
prefix :: Word -> Trie -> Maybe [Word]
prefix w t = (prefix' w t)
    where
        prefix' :: Word -> Trie -> Maybe [Word]
        prefix' [] t' = Just $ map ((++) w) (getWords t')
        prefix' (x:xs) t'
            | (Map.lookup x $ children t') == Nothing = Nothing
            | (end t') == True && xs == []            = Just $ fromJust $ prefix' xs $ fromJust (Map.lookup x $ children t')
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

main = do
    menu <- getMenu
    putStrLn menu
    args <- getArgs
    let filePath = args !! 0
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    let ws = lines contents
        t  = insertList ws
    doAction t
