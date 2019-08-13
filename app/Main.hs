module Main where

import Graphics
import Lib

piles = [ [FaceUp Diamond Queen, FaceUp Diamond King, FaceDown Club Ace]
        , [FaceUp Heart Two, FaceUp Diamond Three, FaceUp Club Four, FaceDown Spade Jack, FaceDown Club Seven]
        , [FaceUp Diamond Ten, FaceDown Club Four, FaceDown Spade Jack, FaceDown Spade King]
        , [FaceUp Heart Three, FaceDown Heart Three, FaceDown Diamond Ten, FaceDown Club Four, FaceDown Spade Jack, FaceDown Spade Ten]
        , [FaceUp Diamond Ace, FaceUp Heart Two]
        , [FaceUp Spade Jack, FaceUp Club Queen, FaceUp Spade King]
        , [FaceUp Heart Queen, FaceDown Club Four]
        ]

main :: IO ()
main = drawPiles piles
  
