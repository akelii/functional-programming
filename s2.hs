import System.Random

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
cardValue c
    | rank c == Ace = 11
    | rank c == Jack = 10
    | rank c == Queen = 10
    | rank c == King = 10
    | otherwise = 2

removeCard :: [Card] -> Card -> [Card]
removeCard [] _    = []
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
                    | length (cardList s)  == 1     = score (head(cardList s):heldCards(s)) g
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

