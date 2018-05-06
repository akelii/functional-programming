import Prelude hiding (Word)
import Data.Char
import Data.List
import Data.Ord
import System.IO
import System.Environment

type Word = String  -- Example data: "Love"
type Sentence = [Word] -- Example data: ["I", "love", "you"]
type CharCount = [(Char, Int)] -- Example data: For "Love" -> [('e', 1), ('l', 1), ('o', 1), ('v', 1)]
type Dictionary = [(Word, CharCount)] -- Example data:  [("Love", ('e', 1), ('l', 1), ('o', 1), ('v', 1))]

{-
cmp function is implemented using the link below (for usage of fst and snd):
stackoverflow.com/questions/2349798/in-haskell-how-can-i-use-the-built-in-sortby-function-to-sort-a-list-of-pairst/2352333#2352333
-}
{-
*** Implementation: 
	1- I found the all char numbers (ns) array. For example ns will be:
		[1,2,2] for word: "all". 
	2- Then I zip ns with cs to obtain ('a', 1), ('l', 2), ('l', 2)
	3- Removes duplicate elements (nub), then sort by alphabetically (first character)
-}
-- Example Input  : "Love"
-- Example Output : [('e',1),('l',1),('o',1),('v',1)] 
wordCharCounts :: Word -> CharCount
wordCharCounts cs = sortBy cmp (nub $ zip cs' ns)
    where
        ns  = map (\x -> length (filter (==x) cs')) cs'
        cs' = map toLower cs
        cmp = comparing fst

{-
*** Implementation: 
	1- Concatenate a list of words to one string (or meaningless word). -> It uses curying
	2- Then call the wordCharCounts
-}
-- Example Input  : ["I", "am", "tea"]
-- Example Output : [('a',2),('e',1),('i',1),('m',1),('t',1)]
sentenceCharCounts :: Sentence -> CharCount
sentenceCharCounts = wordCharCounts . concat

{-
File read operations is implemented using the link below:
learnyouahaskell.com/input-and-output
-}
-- Example Input  : -
-- Example Output : [..., ("accident",[('a',1),('c',2),('d',1),('e',1),('i',1),('n',1),('t',1)]), ...]
dictCharCounts :: IO [(Word, CharCount)]
dictCharCounts = do handle <- openFile "words.txt" ReadMode
                    contents <- hGetContents handle
                    let ws   = lines contents  -- Get lines from content as words(ws) 
                        wccs = zip ws $ map wordCharCounts ws -- WordCharCounts (wccs)
                    return wccs

{-
*** Implementation: 
	1- Filter the (Word, CharCount) pairs according to second pair (CharCount) which is same as with the input CharCount
	2- Then get the first pair for all elements in the list (map fst ..)
-}
-- Example Input  : <result of dictCharCounts (dictionary words)> [('a',1),('e',1),('t',1)]
-- Example Output : ["tea", "eat", "ate"]
dictWordsByCharCounts :: [(Word, CharCount)] -> CharCount -> [Word]
dictWordsByCharCounts [] _  = []
dictWordsByCharCounts xs cc = map fst $ filter (\x -> snd x == cc) xs

{-
Note: Before the implementation of last step, the result of dictWordsByCharCounts was [(CharCount, [Word])]
But after the last implementation (8th step), I don't need wordAnagrams, also function signature of dictWordsByCharCounts
has been changed. 
-}
wordAnagrams :: Word -> [(CharCount, [Word])] -> [Word]
wordAnagrams w (x:xs) = if wordCharCounts w == fst x then snd x else wordAnagrams w xs

-- Example Input  : [('a',1),('l',2)]
-- Example Output : [[],[('l',1)],[('l',2)],[('a',1)],[('a',1),('l',1)],[('a',1),('l',2)]]
charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets ccs = charCountsSubsets' $ extend ccs 
    where
        charCountsSubsets' []          = [[]]
        charCountsSubsets' (x:xs)      = charCountsSubsets' xs ++ filter (not . isIncludesDublicates) (map (sortBy cmp) (map (x:) (charCountsSubsets' xs)))
        isIncludesDublicates []        = False
        isIncludesDublicates [c]       = False
        isIncludesDublicates (c':c:cs) = if fst c' == fst c then True else isIncludesDublicates (c:cs)
        cmp = comparing fst

-- Example Input  : [('a',1),('l',2)]
-- Example Output : [('a',1),('l',2),('l',1)]
extend :: [(Char,Int)] -> [(Char,Int)]
extend []     = []
extend (c:cs) = if snd c > 1 then c:cs' else c:cs''
    where
        cs' = extend (c':cs)
        c'  = (fst c, (snd c) - 1)
        cs''= extend cs

{-
*** Implementation: 
	1- Take the first pair (c) of first input then search (findResult) whether an equal character in second input pairs (cs')
	2- If the result exists take second pair (numeric one) else return default value (maybe 0 ...)
	3- 
		3a- If result 0 (means not exists in the second list) then continue with other pairs
		3b- If substraction of two value (e.g. ('b',2) and ('b',2)) is 0 then search with other pairs without including the newTuple (because it is zero)
		3c- If substraction not zero (means second pair of newTuple is not zero) then continue searching with including the newTuple
-}
-- Example Input  : [('a',1),('l',2)]   [('a',1),('l',1)]
-- Example Output : [('l',1)]
subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts [] _       = []              --                 3a                                                      3b                          3c              
subtractCounts _ []       = []              --                  |                                                       |                           |
subtractCounts (c:cs) cs' = if findResult == 0 then c:(subtractCounts cs cs') else if snd newTuple == 0 then  subtractCounts cs cs' else newTuple:(subtractCounts cs cs')
    where
        findResult   = maybe 0 snd (find (\x -> fst x == fst c) cs')
        newTuple     = (fst c, ((snd c) - findResult))

{-
*** Implementation: 
Baslangicta cumlemizin "I am tea" oldugunu dusunelim. 
	1- getPwwrp fonksiyonunda "iamtea"den olusturulabilecek butun subsetler icin sozlukte anlamlı bir kelime olup olmadigi bakilir.
		Mesela bu subsetlerden [('t',1)],[('m',1)] anlamlı bir kelime vermezken;
		[('a',1),('e',1),('t',1)] subseti tea, eat ve ate kelimelerini verir.
        Anlamlı bir kelime olusturmayan subsetler, dictWordsByCharCounts fonksiyonundan bos kume donecegi icin bos kume olanlara
		gore filtering yapılıp words listesine atilir. (let words = filter (\x -> x /= []) ... )
	2- Herbir kelime icin, ana cumlenin CharCount'dan cikarilması ile remaining CharCount kismi bulunur. (rests)
		Bu iki deger zip lenerek "Kelime - Kalan Kısım" pair'i olusturulur.
	3- Kalan Kısım CharCount'lari icin de bu adım devam ettilerek anlamlı bir kelime olusmayasaya kadar VEYA 
		hic kalan kisim CharCount'u olmasaya kadar bu recursion devam eder. (throughToTail üçlüsü bu recursion'u yapar)
	4- throughToTail'' fonksiyonunda eger arta kalan hic CharCount yoksa o zaman ana cumledeki butun CharCount'lari
		kullanarak kelimeler olusturdugumuz icin bu bizim icin anlamli bir path'dir.
	5- throughToTail' fonksiyonu ise anlamli bir kelime olusturulamayan CharCount'lar icin bos liste doner.
	6- getSentenceWords fonksiyonunda butun wordTree'yi elde ettikten sonra icerisinde "-" icerenler anlamlı path'lerdi
		o yuzden icinde "-" olanlari filtreleyip daha sonra dropDash ile kaldiriyoruz.
		Sonuc olarak sentenceAnagrams fonksiyonunun "map dropDash getSentenceWords" kısmından aşağıdaki gibi bir cikti olusur.
		Bu cikti [[[Word]]] tipindedir.
		[[["ate","eat","tea"],["am"],["i"]],[["Mae"],["at"],["i"]]]
		Yani bu sonucta 
			[["ate","eat","tea"],["am"],["i"]]  ve  [["Mae"],["at"],["i"]] olmak uzere iki farkli cumle olusturulabilir
		Ama [["ate","eat","tea"],["am"],["i"]] sonucunda, aynı CharCount'a sahip farklı kelimeler oldugu icin bunun kombinasyonunu da hesaplanarak
		cumleler icin Word array'lerine karar verilir. 
	7- Kombinasyonları hesaplayan createSentence fonksiyonudur. 
	8- Eger varsa kombinasyon durumlari da hesaplandiktan sonra bir de kelimelerin yerleri degistirilerek olusturulabilecek cumleler elde edilir.
		(addPermutations fonksiyonu)
	9- Kelime arraylari [Word] seklinde olan herbir "Sentence" icin kelimelerin aralarına bosluklar eklenerek String tipinde duzenleme yapilir.
		(createPureSentence)
	10- Elde edilen son [String] tipindeki sonucun birkaç ornegi asagidadir:
		i am ate
		i at Mae
		i eat am
		...
-}
sentenceAnagrams :: Sentence -> Dictionary -> [String]
sentenceAnagrams s dic = createPureSentence $ addPermutations (concat $ map createSentence $ map dropDash getSentenceWords)
    where
        getSentenceWords = filter isIncludesDash (wordTree dic $ sentenceCharCounts s)
        wordTree :: Dictionary -> CharCount -> [[[Word]]] --creates all possible sentences in the form of word tree
        wordTree dic cc =  map throughToTail (getPwwrp cc)
        getPwwrp :: CharCount -> [([Word], CharCount)] --getPwwrp stand for getPossibleWordsWithRemaningPart
        getPwwrp []  = []
        getPwwrp cc' = let words = filter (\x -> x /= []) $ map (dictWordsByCharCounts dic) (charCountsSubsets cc')
                           rests = map (\x -> subtractCounts cc' (wordCharCounts $ x !! 0)) words
                           in zip words rests
        throughToTail :: ([Word], CharCount) -> [[Word]] --collects words to obtain sentence through to tail beyond the one path
        throughToTail wcc = [fst wcc] ++ (throughToTail' $ getPwwrp (snd wcc))
        throughToTail' :: [([Word], CharCount)] -> [[Word]]
        throughToTail' []         = []
        throughToTail' xs@(x:xs') = (throughToTail'' x) ++ [(fst x)]
        throughToTail'' :: ([Word], CharCount) -> [[Word]]
        throughToTail'' (ws,[]) = [["-"]]
        throughToTail'' (_,x)   = throughToTail' $ getPwwrp x
        isIncludesDash []        = False
        isIncludesDash (c:cs) = if c == ["-"] then True else isIncludesDash cs
        dropDash :: [Sentence] -> [Sentence]
        dropDash ss = filter (\x -> x /= ["-"]) ss
{-
*** Implementation: 
	1- "transpose" fonksiyonunu kullanabilmek icin veriyi uygun formata getirmek gerekti. Bunun icin de:
		a- Eger aynı CharCount'a sahip kelime listesi varsa, o zaman diger kelimelerle cumle olusturulabilmesi icin
		genisletilmesi lazım. Ornegin;
			[["ate","eat","tea"],["am"],["i"]] verisi transpose fonksiyonuna;
			[["ate","eat","tea"],["am","am","am"],["i","i","i"]] olarak verilirse
			[["ate","am","i"],["eat","am","i"],["tea","am","i"]] olarak cikar.
	2- Bu yuzden gelen islenmemis verinin "en küçük ortak kat"/"least common multiple" bulunur ve 
		transpose'u alınamdan once (EKOK/Kendi Boyu) oranı seklinde genisletilir.
	3- Mesela "am" gibi "ma"nın da anlamlı bir kelime oldugunu varsayarsak;
			[["ate","eat","tea"],["am","ma"],["i"]] verisi transpose fonksiyonuna;
			[["ate","eat","tea","ate","eat","tea"],["am","ma","am","ma","am","ma"],["i","i","i","i","i","i"]] olarak verilirse
			[["ate","am","i"],["eat","ma","i"],["tea","am","i"],["ate","ma","i"],["eat","am","i"],["tea","ma","i"]] olarak cikar.
		Yani;
			["ate","eat","tea"] -> Uzunluk 3
			["am","ma"]			-> Uzunluk 2
			["i"]				-> Uzunluk 1
			EKOK = 6
		O zaman;
			["ate","eat","tea"] -> 6/3 oranında genisletilir.
			["am","ma"]			-> 6/2 oranında genisletilir.
			["i"]		 		-> 6/1 oranında genisletilir ve transpose fonksiyonuna verilir.
-}
-- Example input  : [["ate","eat","tea"],["am"],["i"]]
-- Example output : [["ate","am","i"],["eat","am","i"],["tea","am","i"]]
createSentence :: [[Word]] -> [Sentence]
createSentence xs@(x:xs') = transpose $ map (\c -> expand (lcm' `div` (length c)) c) xs
    where
        expand :: Int -> [Word] -> [Word]
        expand 1 ws = ws
        expand i ws = ws ++ expand  (i-1) ws
        lcm' = foldr1 (\a b -> lcm a b) (map length xs) -- least common multiple

createPureSentence :: [Sentence] -> [String]
createPureSentence = map $ intercalate " "

addPermutations :: [Sentence] -> [Sentence]
addPermutations ss = concat $ map permutations ss

main = do 
    args <- getArgs
    dic <- dictCharCounts

    let testResult = sentenceAnagrams (words $ args!!0) dic

    mapM_ putStrLn testResult
