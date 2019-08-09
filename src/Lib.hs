module Lib where

import           Control.Applicative
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           System.Random (randomRIO)

data Suit = Spade
          | Club
          | Heart
          | Diamond
          deriving (Eq, Ord, Show, Enum)
  
data Rank = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          deriving (Eq, Ord, Show, Enum)

data Card = FaceUp Suit Rank
          | FaceDown Suit Rank
          | Bottom
  deriving (Eq, Show)

data Cards a = Cards a | NoCards
  deriving (Eq, Show)

instance Monoid a => Monoid (Cards a) where 
  mempty = NoCards
  mappend = (<>)

instance Semigroup a => Semigroup (Cards a) where
  (Cards a) <> (Cards a') = Cards $ a <> a'
  (Cards a) <> NoCards    = Cards a
  NoCards   <> (Cards a)  = Cards a

instance Functor Cards where 
  fmap f (Cards a) = Cards $ f a
  fmap f NoCards   = NoCards

instance Applicative Cards where 
  pure = Cards 
  (Cards f) <*> (Cards a) = Cards $ f a
  (Cards f) <*> (NoCards) = NoCards
  (NoCards) <*> (Cards a) = NoCards 

instance Monad Cards where 
  return = pure 
  (Cards a) >>= f = f a 
  NoCards   >>= f = NoCards

instance Foldable Cards where
  foldMap f (Cards a) = f a 

data Tableau a = Tableau a
             deriving Show

instance Functor Tableau where
  fmap f (Tableau a) = Tableau $ f a 

data Foundations = Foundations (Map Suit (Cards [Card]))
                 deriving Show

data Game = Game 
  { gameTableau :: Tableau (Map (Sum Int) (Cards [Card]))
  , gameStock :: Cards [Card]
  , gameTalon :: Cards [Card]
  , gameFoundations :: Foundations
  } deriving Show

makeStock :: Cards [Card]
makeStock =
  Cards cards 
  where cards = FaceDown <$> suits <*> ranks
        suits = [Spade .. Diamond]
        ranks = [Ace .. King]

flipCard :: Card -> Card 
flipCard (FaceDown s r) = FaceUp s r
flipCard (FaceUp s r) = FaceDown s r
flipCard Bottom = Bottom

flipFirst' :: [Card] -> [Card]
flipFirst' [] = []
flipFirst' (x:xs) = (flipCard x) : xs

flipFirst :: Cards [Card] -> Cards [Card]
flipFirst cards =
  fmap flipFirst' cards

shuffleCard :: (Applicative f, Eq a) => Int -> f [a] -> f [a]
shuffleCard i stock = 
  liftA2 (:) card (liftA2 delete card stock)
  where card = fmap (!! i) stock

shuffleStock :: (Applicative f, Eq a) => f [a] -> IO (f [a])
shuffleStock stock = 
  shuffle 1000 stock
  where shuffle 0 stock' = return stock' 
        shuffle x stock' = do
          i <- randomRIO (1, 51)
          (return $ shuffleCard i stock') >>= (shuffle $ x - 1)

dealOneCard :: Cards [Card] -> [Cards [Card]] -> [Cards [Card]]
dealOneCard _ [] = []
dealOneCard stock (x:xs) = (addCard stock x) : (dealOneCard (takeCard stock) xs)

dealRemaining :: Cards [Card] -> [Cards [Card]] -> [Cards [Card]]
dealRemaining _ [] = []
dealRemaining stock (x:xs) = 
  x : (dealRemaining stock' $ dealOneCard stock' xs)
  where stock' = drop (length $ x:xs) <$> stock

dealTableau' :: Cards [Card] -> [Cards [Card]] -> [Cards [Card]]
dealTableau' stock = 
  (map flipFirst) . ((dealRemaining stock) . (dealOneCard stock))

cardsListToMap :: [Cards [Card]] -> Map (Sum Int) (Cards [Card])
cardsListToMap =
  M.fromList . (map go)
  where go cards = (foldMap (Sum . length) cards, cards)

dealTableau :: Cards [Card] -> Tableau (Map (Sum Int) (Cards [Card]))
dealTableau stock =
  Tableau $ cardsListToMap (dealTableau' stock tableau)
  where tableau = replicate 7 $ Cards []

startGame :: IO Game
startGame = do 
  stock' <- shuffleStock makeStock
  let tableau     = dealTableau stock' 
      stock       = drop 28 <$> stock'
      talon       = Cards []
      foundations = Foundations $ M.fromList $ flip (,) (Cards []) <$> [Spade .. Diamond]
    in return $ Game tableau stock talon foundations

takeCard :: Cards [Card] -> Cards [Card]
takeCard = fmap (drop 1)

addCard :: Cards [Card] -> Cards [Card] -> Cards [Card]
addCard = mappend . fmap (take 1)

isRed :: Suit -> Bool
isRed suit = 
  suit == Heart || suit == Diamond

isBlack :: Suit -> Bool
isBlack = not . isRed

isSuccessor :: Card -> Card -> Bool
isSuccessor cardA cardB =
  case cardA of
    Bottom -> False
    FaceDown _ _ -> False 
    FaceUp suit rank ->
      case cardB of
        Bottom -> False
        FaceDown _ _ -> False
        FaceUp suit' rank' ->
          (succ rank == rank') && (isRed suit == isBlack suit')

isPredecessor :: Card -> Card -> Bool
isPredecessor cardA cardB =
  case cardA of
    Bottom -> False
    FaceDown _ _ -> False 
    FaceUp suit rank ->
      case cardB of
        Bottom -> False
        FaceDown _ _ -> False
        FaceUp suit' rank' ->
          (pred rank == rank') && (isBlack suit == isBlack suit')

isFaceUp :: Card -> Bool
isFaceUp card =
  case card of
    (FaceUp _ _)   -> True
    (FaceDown _ _) -> False
    Bottom         -> False

isFaceDown :: Card -> Bool
isFaceDown card =
  case card of
    (FaceUp _ _)   -> False
    (FaceDown _ _) -> True
    Bottom         -> False

takeFaceUp :: [Card] -> [Card]
takeFaceUp =
  filter isFaceUp

dropFaceUp :: [Card] -> [Card]
dropFaceUp =
  filter isFaceDown

lastOrEmpty :: [Card] -> Card
lastOrEmpty [] = Bottom
lastOrEmpty (x:[]) = x
lastOrEmpty (x:xs) = lastOrEmpty xs

firstOrEmpty :: [Card] -> Card
firstOrEmpty [] = Bottom
firstOrEmpty (x:_) = x

maybeAdjacent :: (Card -> Card -> Bool) -> (Cards [Card], Cards [Card]) -> Maybe (Cards [Card], Cards [Card])
maybeAdjacent f (source, target) =
  let (Cards a) = firstOrEmpty <$> source 
      (Cards b) = firstOrEmpty <$> target 
  in if (f a b)
     then Just (source, target)
     else Nothing

maybeSuccessor :: (Cards [Card], Cards [Card]) -> Maybe (Cards [Card], Cards [Card])
maybeSuccessor = maybeAdjacent isSuccessor

maybePredecessor :: (Cards [Card], Cards [Card]) -> Maybe (Cards [Card], Cards [Card])
maybePredecessor = maybeAdjacent isPredecessor

maybeAdjacentOfSequence :: (Card -> Card -> Bool) -> (Cards [Card], Cards [Card]) -> Maybe (Cards [Card], Cards [Card])
maybeAdjacentOfSequence f (source, target) =
  let (Cards a) = (lastOrEmpty <$> (takeFaceUp <$> source)) 
      (Cards b) = (firstOrEmpty <$> target)
  in if (f a b)
     then Just (source, target)
     else Nothing

maybeSuccessorOfSequence :: (Cards [Card], Cards [Card]) -> Maybe (Cards [Card], Cards [Card])
maybeSuccessorOfSequence = maybeAdjacentOfSequence isSuccessor

transferCard :: (Cards [Card], Cards [Card]) -> (Cards [Card], Cards [Card])
transferCard (source, target) =
  (takeCard source, addCard source target)

transferCards :: (Cards [Card], Cards [Card]) -> (Cards [Card], Cards [Card])
transferCards (source, target) =
  (dropFaceUp <$> source, (takeFaceUp <$> source) <> target)

transferCardToSuccessor :: (Cards [Card], Cards [Card]) -> Maybe (Cards [Card], Cards [Card])  
transferCardToSuccessor = 
  (fmap transferCard) . maybeSuccessor

transferCardToPredecessor :: (Cards [Card], Cards [Card]) -> Maybe (Cards [Card], Cards [Card])  
transferCardToPredecessor = 
  (fmap transferCard) . maybePredecessor

transferSequenceToSuccessor :: (Cards [Card], Cards [Card]) -> Maybe (Cards [Card], Cards [Card])
transferSequenceToSuccessor =
  (fmap transferCards) . maybeSuccessorOfSequence

selectCards :: (Ord k, Monoid a) => k -> Map k a -> a
selectCards key cardPiles =
  case (M.lookup key cardPiles) of
    Just pile -> pile 
    Nothing   -> mempty

transferCardFromStockToTalon :: Game -> Game
transferCardFromStockToTalon game =
  let (updatedStock, updatedTalon) = transferCard (gameStock game, gameTalon game)
  in game { gameStock = updatedStock, gameTalon = flipFirst updatedTalon }

transferCardFromTalonToPile :: Game -> Sum Int -> Game 
transferCardFromTalonToPile game key =
  case result of
    Nothing     -> game 
    Just (updatedTalon, updatedPile) -> 
      let piles = M.insert key updatedPile piles'
      in game { gameTalon = updatedTalon, gameTableau = Tableau piles }
  where (Tableau piles') = gameTableau game
        result          = transferCardToSuccessor (gameTalon game, selectCards key piles')
        
-- the card (if not Bottom) needs to be flipped when card or cards are moved from one pile to another
transferCardsFromPileToPile :: Game -> Sum Int -> Sum Int -> Game 
transferCardsFromPileToPile game key key' =
  case result of 
    Nothing     -> game
    Just (updatedFromPile, updatedToPile) -> 
      let piles = (M.insert key' updatedToPile) . (M.insert key $ flipFirst updatedFromPile) $ piles'
      in game { gameTableau = Tableau piles }
  where (Tableau piles') = gameTableau game 
        result           = transferSequenceToSuccessor (selectCards key piles', selectCards key' piles')

transferCardFromPileToFoundation :: Game -> Sum Int -> Suit -> Game
transferCardFromPileToFoundation game key key' =
  case result of 
    Nothing     -> game 
    Just (updatedPile, updatedFoundation) ->
      let piles       = M.insert key updatedPile piles'
          foundations = M.insert key' updatedFoundation foundations'
      in game { gameTableau     = Tableau  piles
              , gameFoundations = Foundations foundations }
  where (Tableau piles')           = gameTableau game 
        (Foundations foundations') = gameFoundations game
        result                     = transferCardToPredecessor (selectCards key piles', selectCards key' foundations') 
        
transferCardFromTalonToFoundation :: Game -> Suit -> Game
transferCardFromTalonToFoundation game key =
  case result of 
    Nothing     -> game 
    Just (updatedTalon, updatedFoundation) ->
      let f = M.insert key updatedFoundation f'
      in game { gameTalon       = updatedTalon
              , gameFoundations = Foundations f }
  where (Foundations f') = gameFoundations game 
        result           = transferCardToPredecessor (gameTalon game, selectCards key f')

returnTalonToStock :: Game -> Game
returnTalonToStock game =
  game { gameTalon = Cards []
       , gameStock = (map flipCard) . reverse <$> talon' }
  where talon' = gameTalon game
        

