import Prelude hiding (Word)
import Data.Char
import Data.List
import Data.Ord
import System.IO

type Word = String
type Sentence = [Word]
type CharCount = [(Char, Int)]
type Dictionary = [(Word, CharCount)]

{-
cmp function is implemented using the link below (for usage of fst and snd):
stackoverflow.com/questions/2349798/in-haskell-how-can-i-use-the-built-in-sortby-function-to-sort-a-list-of-pairst/2352333#2352333
-}
wordCharCounts :: Word -> CharCount
wordCharCounts cs = sortBy cmp (nub $ zip cs' ns)
    where
        ns  = map (\x -> length (filter (==x) cs')) cs'
        cs' = map toLower cs
        cmp = comparing fst

sentenceCharCounts :: Sentence -> CharCount
sentenceCharCounts = wordCharCounts . concat

{-
File read operations is implemented using the link below:
learnyouahaskell.com/input-and-output
-}
dictCharCounts :: IO [(Word, CharCount)]
dictCharCounts = do handle <- openFile "words.txt" ReadMode
                    contents <- hGetContents handle
                    let ws   = lines contents  -- Get lines from content as words(ws) 
                        wccs = zip ws $ map wordCharCounts ws -- WordCharCounts (wccs)
                    return wccs

dictWordsByCharCounts :: [(Word, CharCount)] -> CharCount -> [Word]
dictWordsByCharCounts [] _  = []
dictWordsByCharCounts xs cc = map fst $ filter (\x -> snd x == cc) xs

wordAnagrams :: Word -> [(CharCount, [Word])] -> [Word]
wordAnagrams w (x:xs) = if wordCharCounts w == fst x then snd x else wordAnagrams w xs

charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets ccs = charCountsSubsets' $ extend ccs 
    where
        charCountsSubsets' []          = [[]]
        charCountsSubsets' (x:xs)      = charCountsSubsets' xs ++ filter (not . isIncludesDublicates) (map (sortBy cmp) (map (x:) (charCountsSubsets' xs)))
        isIncludesDublicates []        = False
        isIncludesDublicates [c]       = False
        isIncludesDublicates (c':c:cs) = if fst c' == fst c then True else isIncludesDublicates (c:cs)
        cmp = comparing fst

extend :: [(Char,Int)] -> [(Char,Int)]
extend []     = []
extend (c:cs) = if snd c > 1 then c:cs' else c:cs''
    where
        cs' = extend (c':cs)
        c'  = (fst c, (snd c) - 1)
        cs''= extend cs

subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts [] _       = []
subtractCounts _ []       = []
subtractCounts (c:cs) cs' = if findResult == 0 then c:(subtractCounts cs cs') else if snd newTuple == 0 then  subtractCounts cs cs' else newTuple:(subtractCounts cs cs')
    where
        findResult   = maybe 0 snd (find (\x -> fst x == fst c) cs')
        newTuple     = (fst c, ((snd c) - findResult))

sentenceAnagrams :: Sentence -> Dictionary -> [[Sentence]]
sentenceAnagrams s dic = map dropDash getSentenceWords
    where
        getSentenceWords = filter isIncludesDash (test dic $ sentenceCharCounts s)
        test :: Dictionary -> CharCount -> [[[Word]]]
        test dic cc =  map test4 (test' cc)
        test' :: CharCount -> [([Word], CharCount)]
        test' []  = []
        test' cc' = let words = filter (\x -> x /= []) $ map (dictWordsByCharCounts dic) (charCountsSubsets cc')
                        rests = map (\x -> subtractCounts cc' (wordCharCounts $ x !! 0)) words
                        in zip words rests
        test4 :: ([Word], CharCount) -> [[Word]]
        test4 wcc = [fst wcc] ++ (test3 $ test' (snd wcc))
        test3 :: [([Word], CharCount)] -> [[Word]]
        test3 []         = []
        test3 xs@(x:xs') = (test2 x) ++ [(fst x)]
        test2 :: ([Word], CharCount) -> [[Word]]
        test2 (ws,[]) = [["-"]]
        test2 (_,x)   = test3 $ test' x
        isIncludesDash []        = False
        isIncludesDash (c:cs) = if c == ["-"] then True else isIncludesDash cs
        dropDash :: [Sentence] -> [Sentence]
        dropDash ss = filter (\x -> x /= ["-"]) ss

--Example input for test: ["I", "love", "you"]
main = do 
    putStrLn "Hello"
    dic <- dictCharCounts
    line <- getLine

    let ss = read line :: Sentence
    let testResult = sentenceAnagrams ss dic
    print testResult
