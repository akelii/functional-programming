import Data.Char

data Color = Red | Black 
    deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq)
data Rank = Num Int | Jack | Queen | King | Ace
    deriving (Show, Eq)
data Card = Card { suit :: Suit, rank :: Rank }
    deriving (Show, Eq)
data Move = Draw | Discard Card
    deriving (Show, Eq)
data State = State { cardList :: [Card], heldCards :: [Card], moveList :: [Move], goal :: Int }
    deriving (Show, Eq)

cardColor :: Card -> Color
cardColor c
    | suit c == Clubs = Black
    | suit c == Diamonds = Red
    | suit c == Hearts = Red
    | otherwise = Black

cardValue :: Card -> Int
cardValue c = case rank c of
                Num x -> x
                Ace   -> 11
                Jack  -> 10
                Queen -> 10
                King  -> 10

removeCard :: [Card] -> Card -> [Card]
removeCard [] _    = error "Card list is empty!"
removeCard [a] c'
    | a == c'   = []
    | otherwise = error "The card is not in list!"
removeCard (c:cs) c' 
    | c' == c   =  cs
    | otherwise = c : removeCard cs c'

allSameColor :: [Card] -> Bool
allSameColor [] = error "Empty List!"
allSameColor [c] = True
allSameColor (c':c:cs)
    | cardColor c' == cardColor c = allSameColor (c:cs)
    | otherwise                   = False

sumCards :: [Card] -> Int
sumCards (c:cs) = helperSum 0 (c:cs)
    where
        helperSum :: Int -> [Card] -> Int
        helperSum acc []       = acc
        helperSum acc (c:cs) = helperSum (acc + cardValue c) cs 

score :: [Card] -> Int -> Int
score [] _                   = 0
score (c:cs) goal
    | sumCards (c:cs) > goal = checkColorResult(3 * (sumCards (c:cs) - goal)) (c:cs)
    | otherwise              = checkColorResult(goal - sumCards (c:cs)) (c:cs) 
        where
            checkColorResult :: Int -> [Card] -> Int
            checkColorResult preliminary (c:cs)
                | allSameColor (c:cs) == True = preliminary `div` 2
                | otherwise                   = preliminary

runGame :: [Card] -> [Move] -> Int -> Int
runGame (c:cs) ms g = runState State {cardList=(c:cs), heldCards=[], goal=g, moveList=ms}
    where 
        runState :: State -> Int
        runState s
            | moveList s == [] = score (heldCards s) g
            | otherwise        = if head (moveList s) == Draw then makeDraw else makeDiscard (head (moveList s))
            where
                makeDraw :: Int
                makeDraw
                    | length (cardList s)  == 0     = score (heldCards s) g
                    | sumCards (head(cardList s):heldCards(s)) > g = score (head(cardList s):heldCards(s)) g
                    | otherwise                     = runState State {goal=g, moveList=tail (moveList s), cardList=tail (cardList s), heldCards=(head (cardList s):heldCards(s))}
                makeDiscard :: Move -> Int
                makeDiscard m = case m of
                    Discard c -> runState State {goal=g, moveList=tail (moveList s), cardList=cardList s, heldCards=removeCard (heldCards s) c}

convertSuit :: Char -> Suit
convertSuit c
    | c `elem` "dD" = Diamonds
    | c `elem` "cC" = Clubs
    | c `elem` "hH" = Hearts
    | c `elem` "sS" = Spades
    | otherwise = error "Unknown Suit"

convertRank :: Char -> Rank
convertRank c
    | c `elem` "jJ" = Jack
    | c `elem` "qQ" = Queen
    | c `elem` "kK" = King
    | c `elem` "tT" = Num 10
    | isDigit c     = if digitToInt c == 1 then Ace else Num (digitToInt c)
    | otherwise     = error "Unknown Rank"

convertCard :: Char -> Char -> Card
convertCard s r = Card {suit=convertSuit s, rank=convertRank r}

{- The readCards implementation is improved by using 
stackoverflow.com/questions/49697277/read-io-string-and-return-custom-data-type-list-in-haskell/49697507 -}
readCards :: IO [Card]
readCards = returnCardList []
    where 
        returnCardList :: [Card] -> IO [Card]
        returnCardList acc =  do line <- getLine
                                 if line == "."
                                 then return acc
                                 else returnCardList (acc ++ [(convertCard (line !! 0) (line !! 1))])

convertMove :: Char -> Char -> Char -> Move
convertMove name suit rank
    | name `elem` "dD" = Draw
    | name `elem` "rR" = Discard (convertCard suit rank)
    | otherwise        = error "Invalid Move"

readMoves :: IO [Move]
readMoves = returnMoveList []
    where 
        returnMoveList :: [Move] -> IO [Move]
        returnMoveList acc =  do line <- getLine
                                 if line == "."
                                 then return acc
                                 else returnMoveList (acc ++ [(convertMove (line !! 0) (line !! 1) (line !! 2))])

main = do putStrLn "Enter cards:"
          cards <- readCards
          putStrLn (show cards)

          putStrLn "Enter moves:"
          moves <- readMoves
          putStrLn (show moves)

          putStrLn "Enter goal:"
          line <- getLine

          let goal = read line :: Int

          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)
