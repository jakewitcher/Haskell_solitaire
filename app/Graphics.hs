module Graphics where

import Lib

cardTop :: String
cardTop    = "............"

cardBottom :: String
cardBottom = "''''''''''''"

cardRankTop' :: String -> String
cardRankTop' "10" = "|10        |" 
cardRankTop' str  = "|" ++ str ++ "         |"

cardRankTop :: Rank -> String
cardRankTop rank =
  case rank of 
    Ace   -> cardRankTop' "A"
    Two   -> cardRankTop' "2"
    Three -> cardRankTop' "3"
    Four  -> cardRankTop' "4"
    Five  -> cardRankTop' "5"
    Six   -> cardRankTop' "6"
    Seven -> cardRankTop' "7"
    Eight -> cardRankTop' "8"
    Nine  -> cardRankTop' "9"
    Ten   -> cardRankTop' "10"
    Jack  -> cardRankTop' "J"
    Queen -> cardRankTop' "Q"
    King  -> cardRankTop' "K"

cardRankBottom' :: String -> String
cardRankBottom' "10" = "|        10|"
cardRankBottom' rank = "|         " ++ rank ++ "|"

cardRankBottom :: Rank -> String
cardRankBottom rank =
  case rank of
    Ace   -> cardRankBottom' "A"
    Two   -> cardRankBottom' "2"
    Three -> cardRankBottom' "3"
    Four  -> cardRankBottom' "4"
    Five  -> cardRankBottom' "5"
    Six   -> cardRankBottom' "6"
    Seven -> cardRankBottom' "7"
    Eight -> cardRankBottom' "8"
    Nine  -> cardRankBottom' "9"
    Ten   -> cardRankBottom' "10"
    Jack  -> cardRankBottom' "J"
    Queen -> cardRankBottom' "Q"
    King  -> cardRankBottom' "K"

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

printSuit :: Suit -> IO ()
printSuit suit =
  case suit of 
    Club -> do
      putStrLn clubTop
      putStrLn clubBottom
    Spade -> do 
      putStrLn spadeTop
      putStrLn spadeBottom
    Heart -> do 
      putStrLn heartTop
      putStrLn heartBottom
    Diamond -> do 
      putStrLn diamondTop
      putStrLn diamondBottom

printCardMiddle :: Suit -> Rank -> IO ()
printCardMiddle suit rank =
  case rank of
    King -> do
      putStrLn $ kingCrown suit 
      putStrLn $ faceCardHead suit 
      putStrLn faceCardRoyalShoulders
      putStrLn faceCardWaist
    Queen -> do 
      putStrLn $ queenCrown suit 
      putStrLn $ faceCardHead suit 
      putStrLn faceCardRoyalShoulders
      putStrLn faceCardWaist
    Jack -> do 
      putStrLn $ jackHat suit 
      putStrLn $ faceCardHead suit 
      putStrLn faceCardJackShoulders
      putStrLn faceCardWaist
    _ -> do 
      putStrLn cardEmptySpace
      printSuit suit
      putStrLn cardEmptySpace

printCard :: Suit -> Rank -> IO ()
printCard suit rank = do 
  putStrLn cardTop
  putStrLn $ cardRankTop rank
  printCardMiddle suit rank
  putStrLn $ cardRankBottom rank
  putStrLn cardBottom

printCardBack :: IO ()
printCardBack = do
  putStrLn cardTop 
  putStrLn cardBackStars
  putStrLn cardBackRow1
  putStrLn cardBackRow2
  putStrLn cardBackRow3
  putStrLn cardBackRow4
  putStrLn cardBackStars
  putStrLn cardBottom
