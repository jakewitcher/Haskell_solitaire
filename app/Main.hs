module Main where

import Graphics
import Lib

main :: IO ()
main = do
  printCard Club Ace
  printCard Heart Queen
  printCard Diamond Jack
  printCard Spade King
  printCard Club Ten
  printCard Heart Seven
  printCard Diamond Three
  printCard Spade Nine
  printCardBack
