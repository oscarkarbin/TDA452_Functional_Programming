module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random
--A0
-- hand2 is example hand used in development
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Ace Spades) Empty)
hand1 = Add (Card (Numeric 7) Diamonds) (Add (Card Queen Clubs) (Add (Card King Hearts) Empty))
--sizeSteps shows the amount of steps in the size function
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            -- ... add the remaining steps here
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            ,2]

--A1
--Given a hand, display shows the hand in a nice format
display :: Hand -> String
display Empty = ""
display (Add card hand) = displayCard card ++ "\n" ++ display hand

--displayCard shows a single card. It is used in the function above
displayCard :: Card -> String
displayCard (Card (Numeric n) s) = show n ++ " of " ++ show s  
displayCard (Card r s)           = show r ++ " of " ++ show s

--A2
--This function gives the value of a card rank. It gives a value of 11 for Aces.
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank rank | rank == Ace = 11
               | otherwise   = 10

-- Computes the initial value of a hand in Blackjack.
-- The initial value is the sum of the card values in the hand,
-- where Aces are counted as 11, and face cards are counted as 10.
initialValue :: Hand -> Integer
initialValue Empty           = 0 -- An empty hand has a value of 0.
initialValue (Add card hand) = valueRank (rank card) + initialValue hand 

-- Counts the number of Aces in a hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0 -- An empty hand has 0 aces
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand 
                             | otherwise        = numberOfAces hand 

-- Calculates the value of a Blackjack hand.
-- If the initial value is over 21 and there are Aces in the hand, it adjusts the Aces' value from 11 to 1
-- by subtracting 10 for each Ace.
value :: Hand -> Integer
value Empty = 0 -- The value of an empty hand is 0.
value hand | initialValue hand > 21 = initialValue hand - (numberOfAces hand * 10) 
           | otherwise              = initialValue hand 

--A3
-- Determines if the game is over for the given hand in Blackjack.
-- The game is considered over if the hand's value exceeds 21, which is a 'bust'.
gameOver :: Hand -> Bool
gameOver Empty = False
gameOver hand | value hand > 21 = True
              | otherwise       = False

--A4
-- Determines the winner between two hands in a game of Blackjack: the guest's hand (g) and the bank's hand (b).
-- The winner is decided based on the values of the hands and whether either hand has gone over 21 ('bust').
winner :: Hand -> Hand -> Player
winner g b | gameOver g                        = Bank
           | gameOver b && not (gameOver g)    = Guest 
           | value g > value b                         = Guest
           | otherwise                                 = Bank

--B1
-- a function for a inifix opperator that stacks two hands on eachother.
(<+) :: Hand -> Hand -> Hand 
(<+) Empty Empty = Empty
(<+) Empty h2 = h2
(<+) h1 Empty = h1
(<+) (Add card h1) h2 = Add card (h1 <+ h2)

--Test for (<+)
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

--Test for (<+)
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

--B2
--  a function that creates a full deck of cards
fullDeck :: Hand
fullDeck = foldr Add Empty [Card r s | r <- allRanks, s <- allSuits]

--  a list of all possible ranks
allRanks :: [Rank]
allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

--  a list of all possible suits
allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

--B3
--  a function that will draw one card from the deck and put it on the hand.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck , Add card hand) 

--B4

-- a function that acts as the bank. It will only work with an Empty hand
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty


-- a helper function for the playBank function. It will check that the value of the banks hand does not exceed 16.
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand | value hand >= 16 = hand
                         | otherwise        = playBankHelper smallerDeck biggerHand
                            where (smallerDeck, biggerHand) = draw deck hand


--B5
removeNth :: Hand -> Integer -> (Hand, Card)
removeNth Empty _           = error "The deck is empty"
removeNth (Add card hand) n 
    | n /= 0    = (Add card newHand , newCard)
    | otherwise = (hand,card)
        where (newHand, newCard) = removeNth hand (n-1)

shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty = Empty
shuffleDeck g deck = shuffleDeckHelper g deck Empty

shuffleDeckHelper :: StdGen -> Hand -> Hand -> Hand
shuffleDeckHelper g Empty newDeck   = newDeck
shuffleDeckHelper g oldDeck newDeck = 
    let (n, g1) = randomR (0, size oldDeck - 1) g
        (reducedHand, nthCard) = removeNth oldDeck n
    in shuffleDeckHelper g1 reducedHand (Add nthCard newDeck)


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

belongsTo :: Card -> Hand -> Bool 
c `belongsTo` Empty = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)

--B6
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
}

main :: IO () 
main = runGame implementation