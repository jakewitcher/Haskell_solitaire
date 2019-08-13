module Graphics where

import Data.List
import Lib

cardTop :: String
cardTop    = "............"

cardBottom :: String
cardBottom = "''''''''''''"

cardRankTop' :: String -> String
cardRankTop' "10" = "|10        |" 
cardRankTop' str  = "|" ++ str ++ "         |"

cardRankBottom' :: String -> String
cardRankBottom' "10" = "|        10|"
cardRankBottom' rank = "|         " ++ rank ++ "|"

cardRank :: (String -> String) -> Rank -> String
cardRank f rank =
  case rank of 
    Ace   -> f "A"
    Two   -> f "2"
    Three -> f "3"
    Four  -> f "4"
    Five  -> f "5"
    Six   -> f "6"
    Seven -> f "7"
    Eight -> f "8"
    Nine  -> f "9"
    Ten   -> f "10"
    Jack  -> f "J"
    Queen -> f "Q"
    King  -> f "K"

cardRankTop :: Rank -> String
cardRankTop =
  cardRank cardRankTop'

cardRankBottom :: Rank -> String
cardRankBottom =
  cardRank cardRankBottom'

cardEmptySpace :: String
cardEmptySpace = "|          |"

spadeTop :: String
spadeTop = "|    /\\    |"

spadeBottom :: String 
spadeBottom = "|   (  )   |"

clubTop :: String
clubTop = "|    ()    |"

clubBottom :: String 
clubBottom = "|   ()()   |"

diamondTop :: String 
diamondTop = "|    /\\    |"

diamondBottom :: String
diamondBottom = "|    \\/    |"

heartTop :: String
heartTop = "|   ()()   |"

heartBottom :: String 
heartBottom = "|    \\/    |"

kingCrown :: Suit -> String
kingCrown suit =
  case suit of
    Spade   -> "| /\\   \\M/ |"
    Club    -> "| ()   \\M/ |"
    Heart   -> "|()()  \\M/ |"
    Diamond -> "| /\\   \\M/ |"

queenCrown :: Suit -> String
queenCrown suit =
  case suit of
    Spade   -> "| /\\   \\V/ |"
    Club    -> "| ()   \\V/ |"
    Heart   -> "|()()  \\V/ |"
    Diamond -> "| /\\   \\V/ |"

jackHat :: Suit -> String
jackHat suit =
  case suit of
    Spade   -> "| /\\   [=] |"
    Club    -> "| ()   [=] |"
    Heart   -> "|()()  [=] |"
    Diamond -> "| /\\   [=] |"

faceCardHead :: Suit -> String
faceCardHead suit =
  case suit of
    Spade   -> "|(  )  _U_ |"
    Club    -> "|()()  _U_ |"
    Heart   -> "| \\/   _U_ |"
    Diamond -> "| \\/   _U_ |"

faceCardRoyalShoulders :: String 
faceCardRoyalShoulders = "|     /| |\\|"

faceCardJackShoulders :: String 
faceCardJackShoulders = "|     /\\_/\\|"

faceCardWaist :: String
faceCardWaist = "|     \\| |/|"

cardBackStars :: String
cardBackStars = "|**********|"

cardBackRow1 :: String
cardBackRow1 = "|***({})***|"

cardBackRow2 :: String 
cardBackRow2 = "|**){}{}(**|"

cardBackRow3 :: String 
cardBackRow3 = "|**({}{})**|"

cardBackRow4 :: String 
cardBackRow4 = "|***({})***|"

emptySpace :: String 
emptySpace = "            "

drawSuit :: Suit -> [String]
drawSuit suit =
  case suit of 
    Club -> [clubTop, clubBottom]
    Spade -> [spadeTop, spadeBottom]
    Heart -> [heartTop, heartBottom]
    Diamond -> [diamondTop, diamondBottom]

drawCardMiddle :: Suit -> Rank -> [String]
drawCardMiddle suit rank =
  case rank of
    King  -> [kingCrown suit, faceCardHead suit, faceCardRoyalShoulders, faceCardWaist]
    Queen -> [queenCrown suit, faceCardHead suit, faceCardRoyalShoulders, faceCardWaist]
    Jack  -> [jackHat suit, faceCardHead suit, faceCardJackShoulders, faceCardWaist]
    _     -> [cardEmptySpace] ++ drawSuit suit ++ [cardEmptySpace]

drawCardFace :: Suit -> Rank -> [String]
drawCardFace suit rank =
  [cardTop, cardRankTop rank] ++ drawCardMiddle suit rank ++ [cardRankBottom rank, cardBottom]

drawCardBack :: [String]
drawCardBack =
  [cardTop, cardBackStars, cardBackRow1, cardBackRow2, cardBackRow3, cardBackRow4, cardBackStars, cardBottom]

drawEmpty :: [String]
drawEmpty =
  [cardTop, cardEmptySpace, cardEmptySpace, cardEmptySpace, cardEmptySpace, cardEmptySpace, cardEmptySpace, cardBottom]

drawCardFaceInPile :: Rank -> [String]
drawCardFaceInPile rank =
  [cardTop, cardRankTop rank]

drawCardBackInPile :: [String]
drawCardBackInPile =
  [cardTop, cardBackStars]

drawFrontCard :: Card -> [String]
drawFrontCard (FaceUp suit rank) =
  drawCardFace suit rank

drawRemaining :: Card -> [String]
drawRemaining card =
  case card of
    FaceDown _ _  -> drawCardBackInPile
    FaceUp _ rank -> drawCardFaceInPile rank
    Bottom        -> drawEmpty

-- TODO: Using init and last with checking if the list is empty will crash program
drawPile :: [Card] -> [String]
drawPile cards =
  concatMap drawRemaining (init cards) ++ drawFrontCard (last cards)

drawPiles :: [[Card]] -> IO ()
drawPiles piles =
  let x = transpose $ matchMaxLength $ map (drawPile . reverse) piles
      y = map (intercalate "  ") x
  in foldMap putStrLn y

matchMaxLength :: [[String]] -> [[String]]
matchMaxLength cards =
  map (addLength maxLength) cards
  where maxLength = foldr (max . length) 0 cards

addLength :: Int -> [String] -> [String]
addLength expected cards
  | length cards == expected = cards
  | otherwise = addLength expected (addEmptySpace cards)

addEmptySpace :: [String] -> [String]
addEmptySpace cards =
  cards ++ [emptySpace]