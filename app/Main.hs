module Main where

import Graphics
import Lib

piles = [ [Left (Card Diamond King), Left (Card Diamond Eight), Left (Card Club Ace), Right (Card Heart Two)]
        , [Left (Card Diamond Ten), Left (Card Club Four), Left (Card Spade Jack), Right (Card Club Seven)]
        , [Left (Card Diamond Ten), Left (Card Club Four), Left (Card Spade Jack), Right (Card Spade King)]
        , [Left (Card Diamond Ten), Left (Card Club Four), Left (Card Spade Jack), Right (Card Spade Ten)]
        , [Left (Card Diamond Ten), Left (Card Club Four), Left (Card Spade Jack), Right (Card Heart Three)]
        , [Left (Card Diamond Ten), Left (Card Club Four), Left (Card Spade Jack), Right (Card Heart Jack)]
        , [Left (Card Diamond Ten), Left (Card Club Four), Left (Card Spade Jack), Right (Card Heart Queen)]
        ]

main :: IO ()
main = do
  drawPiles piles
  
