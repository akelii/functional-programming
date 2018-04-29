import Prelude hiding (Word)
import Data.Char
import Data.List
import Data.Ord
import System.IO

type Word = String
type Sentence = [Word]
type CharCount = [(Char, Int)]

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

dictWordsByCharCounts :: [(Word, CharCount)] -> [(CharCount, [Word])]
dictWordsByCharCounts wccs = map wfcc (nub $ map (\x -> filter ((==x).snd) wccs) (map snd wccs))
    where
        wfcc :: [(Word, CharCount)] -> (CharCount, [Word]) --wfcc stand for WordsFromCharCount
        wfcc ps@(p:ps') = (snd p, map fst ps)
