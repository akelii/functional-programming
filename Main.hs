import Prelude hiding (Word)
import Data.Char
import Data.List

type Word = String
type Sentence = [Word]
type CharCount = [(Char, Int)]

wordCharCounts :: Word -> CharCount
wordCharCounts cs = nub (zip cs' ns)
    where
        ns  = map (\x -> length (filter (==x) cs')) cs'
        cs' = map toLower cs

sentenceCharCounts :: Sentence -> CharCount
sentenceCharCounts = wordCharCounts . concat
