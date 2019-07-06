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

instance Semigroup Suit where 
  Spade   <> _ = Spade
  Club    <> _ = Club
  Heart   <> _ = Heart 
  Diamond <> _ = Diamond 
  
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
          deriving (Eq, Show, Enum, Ord)

data Card = Card Suit Rank | Bottom
  deriving (Eq, Show)

instance Ord Card where 
  (Card _ a) <= (Card _ a') = a <= a'

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
  where cards = Card <$> suits <*> ranks
        suits = [Spade .. Diamond]
        ranks = [Ace .. King]

shuffleCard :: (Applicative f, Eq a) => Int -> f [a] -> f [a]
shuffleCard i d = 
  (:) <$> c <*> (delete <$> c <*> d)
  where c = (!! i) <$> d

shuffleStock :: (Applicative f, Eq a) => f [a] -> IO (f [a])
shuffleStock d = 
  shuffle 1000 d
  where shuffle 0 d' = return d' 
        shuffle x d' = do
          i <- randomRIO (1, 51)
          (return $ shuffleCard i d') >>= (shuffle $ x - 1)

dealTableau :: Cards [a] -> Tableau (Map (Sum Int) (Cards [a]))
dealTableau stock =
  Tableau $ M.fromList $ map (\(Cards x) -> (Sum $ length x, Cards x)) $ deal t
  where t = replicate 7 $ Cards []
        dealOneCard _ [] = []
        dealOneCard st (x:xs) = ((take 1 <$> st) <> x) : (dealOneCard (drop 1 <$> st) xs)
        buildTableau _ [] = [] 
        buildTableau st (x:xs) = x : (buildTableau st' $ dealOneCard st' xs)
          where st' = drop (length $ x:xs) <$> st
        deal = (buildTableau stock) . (dealOneCard stock)

startGame :: IO Game
startGame = do 
  s <- shuffleStock makeStock
  let tableau     = dealTableau s 
      stock       = drop 28 <$> s
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
isSuccessor (Card _ King) Bottom = True
isSuccessor (Card _ King) _      = False
isSuccessor Bottom _             = False
isSuccessor (Card suit rank) (Card suit' rank') =
  (succ rank == rank') && (isRed suit == isBlack suit') 

isPredecessor :: Card -> Card -> Bool
isPredecessor (Card _ Ace) Bottom = True
isPredecessor (Card _ Ace) _      = False
isPredecessor Bottom _            = False
isPredecessor (Card suit rank) (Card suit' rank') =
  (pred rank == rank') && (isBlack suit == isBlack suit')

takeSequence :: [Card] -> [Card]
takeSequence (x:[]) = []
takeSequence (x:x':xs) =
  if (isSuccessor x x')
  then x : (takeSequence (x':xs)) 
  else [x]

dropSequence :: [Card] -> [Card]
dropSequence (x:[]) = []
dropSequence (x:x':xs) = 
  if (isSuccessor x x')
  then (dropSequence (x':xs))
  else (x':xs)

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
  let (Cards a) = (lastOrEmpty <$> (takeSequence <$> source)) 
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
  (dropSequence <$> source, (takeSequence <$> source) <> target)

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
  let (a, b) = transferCard (gameStock game, gameTalon game)
  in game { gameStock = a, gameTalon = b }

transferCardFromTalonToPile :: Game -> Sum Int -> Game 
transferCardFromTalonToPile game k =
  case result of
    Nothing     -> game 
    Just (a, b) -> 
      let p = M.insert k b p'
      in game { gameTalon = a, gameTableau = Tableau p }
  where (Tableau p') = gameTableau game
        result       = transferCardToSuccessor (gameTalon game, selectCards k p')
        

transferCardsFromPileToPile :: Game -> Sum Int -> Sum Int -> Game 
transferCardsFromPileToPile game k k' =
  case result of 
    Nothing     -> game
    Just (a, b) -> 
      let piles = (M.insert k' b) . (M.insert k a) $ piles'
      in game { gameTableau = Tableau piles }
  where (Tableau piles') = gameTableau game 
        result           = transferSequenceToSuccessor (selectCards k piles', selectCards k' piles')

transferCardFromPileToFoundation :: Game -> Sum Int -> Suit -> Game
transferCardFromPileToFoundation game k k' =
  case result of 
    Nothing     -> game 
    Just (a, b) ->
      let piles       = M.insert k a piles'
          foundations = M.insert k' b foundations'
      in game { gameTableau     = Tableau  piles
              , gameFoundations = Foundations foundations }
  where (Tableau piles')           = gameTableau game 
        (Foundations foundations') = gameFoundations game
        result                     = transferCardToPredecessor (selectCards k piles', selectCards k' foundations') 
        
transferCardFromTalonToFoundation :: Game -> Suit -> Game
transferCardFromTalonToFoundation game k =
  case result of 
    Nothing     -> game 
    Just (a, b) ->
      let f = M.insert k b f'
      in game { gameTalon       = a
              , gameFoundations = Foundations f }
  where (Foundations f') = gameFoundations game 
        result           = transferCardToPredecessor (gameTalon game, selectCards k f')
