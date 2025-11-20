module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

-- | Additional test hands

hand3 = Add (Card Ace Hearts) (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty))

hand4 = Add (Card (Numeric 3) Hearts)
            (Add (Card Jack Spades) Empty)

bustHand = Add (Card (Numeric 10) Hearts)
            (Add (Card King Spades)
            (Add (Card (Numeric 5) Diamonds) Empty))

hand5 = Add (Card (Numeric 2) Hearts)
            (Add (Card (Numeric 3) Diamonds)
            (Add (Card (Numeric 4) Spades)
            (Add (Card (Numeric 5) Clubs)
            (Add (Card (Numeric 6) Hearts) Empty))))

-- | A0. All recursive steps of the size function 
sizeSteps :: [Integer]
sizeSteps = [ size hand2, 
            size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            ,1 + size (Add (Card Jack Spades) Empty)
            ,2 + size Empty
            ,2]

-- | A1. Shows the cards in it in a nice format.

-- | Displays the card in a nice format, e.g "Jack of Spades"
displayCard :: Card -> String
displayCard card = displayRank (rank card) ++ " of " ++ show(suit card) 
    where 
        displayRank :: Rank -> String
        displayRank (Numeric n) = show n
        displayRank r = show r

-- | Displays the cards of a hand on multiple lines
display :: Hand -> String
display Empty = ""
display (Add card hand) = displayCard (card) ++ "\n" ++ display hand

-- | A2: Compute the value of a hand

--  | Computes the initial value of a hand with Aces counted as 11
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueRank (rank card) + initialValue hand
    where 
        valueRank :: Rank -> Integer
        valueRank (Numeric n) = n
        valueRank Ace         = 11
        valueRank _           = 10 

-- | Calculates the number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty          = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand)   = numberOfAces hand

-- Calculates the value of a hand, and counts aces as 1 if the initial value exceeds 21
value :: Hand -> Integer
value hand 
    | initial > 21 = initial - (10 * numberOfAces hand)
    | otherwise    = initial
    where 
        initial  = initialValue hand

-- | A3: Is the player bust?

-- Tells if the value of a hand exceeds 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- | A4: Who won?

-- | Given the hands of the guest and the bank, determines the winner based on the BlackJack rules
-- | Rules:
-- | 1. If Guest is not bust, and Bank is bust => Guest wins
-- | 2. If Guest is not bust, and Guest's value > Bank's value => Guest wins
-- | 3. Otherwise, Bank wins (e.g. both bust, or Bank not bust and its value >= Guest's value)
winner :: Hand -> Hand -> Player
winner guest bank 
    | guestValue <= 21 && (bankValue > 21 || guestValue > bankValue) = Guest
    | otherwise = Bank
    where guestValue = value guest
          bankValue = value bank

---- Section B ----

-- | B1: Puts one hand above the other
(<+) :: Hand -> Hand -> Hand
Empty <+ second = second
(Add card restOfFirst) <+ second = Add card (restOfFirst <+ second)

-- Verifies that placing a hand on top of another is an associative operator
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

-- Verifies that the size of combined hands is the sum of the sizes of each hand
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf first second = size (first <+ second) == (size first + size second)

-- | B2: Builds a full deck of 52 cards

rank_numerics = [ (Numeric n) | n <- [2..10] ]
rank_faces    = [ Jack, Queen, King, Ace]
rank_all = rank_numerics ++ rank_faces
-- List of all combinations of cards
all_cards = [ Card rank suit | rank <- rank_all, suit <- [Hearts, Spades, Diamonds, Clubs]]

constructHand :: [Card] -> Hand
constructHand [] = Empty
constructHand (card:cards) = Add card (constructHand cards)

-- Constructs a hand with all available cards
fullDeck :: Hand
fullDeck = constructHand all_cards

-- | B3: Given a deck and a hand, draw one card from the deck and put on the hand.
-- |    Return both the deck and the hand (in that order). 

-- Draws one card from the deck and adds it to the hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (newDeck, newHand)
    where
        newDeck = deck 
        newHand = Add card hand

-- | B4: Given a deck, play for the bank according to the rules above (starting with an empty hand)
-- |     Return the bank’s final hand.

-- The bank draws cards until its value reaches at least 16
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty
    where 
        playBankHelper deck hand 
            | (value biggerHand) < 16 = playBankHelper smallerDeck biggerHand
            | otherwise = biggerHand
                where (smallerDeck,biggerHand) = draw deck hand

-- | B5: Shuffles the given hand/deck


-- Shuffles the given hand using the provided random generator
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g hand = shuffleDeckHelper g hand Empty (size hand) -- Start with empty accumulation hand, and pick one card at a time from the given hand
    where 
        -- Moves i cards from hand to acc in random order
        -- the index i indicates how many cards are left to pick from the hand
        shuffleDeckHelper :: StdGen -> Hand -> Hand -> Int -> Hand
        shuffleDeckHelper g hand acc i
            | i == 0    = acc -- When no cards left to pick, returns the accumulated & shuffled hand 
            | otherwise = shuffleDeckHelper g' restAfterRemoval (Add removed acc) (i-1) -- Recurses to pick next card

            where 
                (r, g') = randomR (1, i) g -- Picks a random index from 1 to i
                (removed, restAfterRemoval) = removeNthCard r hand -- Remove the nth card from the hand  

-- Removes the n:th card of the hand and returns (removedCard, restCards)
-- n ranges from 1 to (size hand)
removeNthCard :: Int -> Hand -> (Card, Hand)
removeNthCard _ Empty = error "removeNthCard: The hand is empty."
removeNthCard 1 (Add c rest) = (c, rest)  -- Base case: removes the first card
removeNthCard n (Add c rest) = (removed, Add c restAfterRemoval) -- Recurses to find the nth card
    where 
        (removed, restAfterRemoval) = removeNthCard (n-1) rest


-- Checks if the card belongs to the hand
belongsTo :: Card -> Hand -> Bool 
c `belongsTo` Empty = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Verifies that shuffling a 
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- Verifies that the size of the hand remains the same after shuffling
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffleDeck g hand)

-- B6: Implementation of the game

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