module Spec where

import           Data.List
import qualified Data.Map as M
import           Data.Monoid
import           Lib
import           Test.Hspec

deck  = FaceDown <$> [Spade .. Diamond] <*> [Ace .. King]
pileA = Cards . (take 10) $ deck
pileB = Cards . (take 10) . reverse $ deck

pile1 = Cards [FaceUp Club Ace, FaceUp Heart Two, FaceUp Club Three, FaceDown Diamond King, FaceDown Diamond Eight]
pile2 = Cards [FaceUp Spade Jack, FaceDown Spade Nine, FaceDown Diamond Queen, FaceDown Heart Three]
pile3 = Cards [FaceUp Spade Eight, FaceDown Club Two, FaceDown Club Jack]
pile4 = Cards []
pile5 = Cards [FaceUp Heart Four, FaceUp Spade Five, FaceUp Diamond Six, FaceUp Spade Seven]
pile6 = Cards [FaceUp Diamond Ace, FaceDown Spade King, FaceDown Heart King]
pile7 = Cards [FaceUp Diamond Two, FaceDown Club King, FaceDown Heart Ace, FaceDown Diamond Ten]

foundation1 = Cards []
foundation2 = Cards [FaceUp Spade Two, FaceUp Spade Ace]
foundation3 = Cards []
foundation4 = Cards []

testStock = Cards [ FaceDown Club Four, FaceDown Diamond Five, FaceDown Spade Two, FaceDown Club Six, FaceDown Club Seven, FaceDown Club Nine
                  , FaceDown Club Queen, FaceDown Spade Four, FaceDown Spade Six, FaceDown Spade Ten, FaceDown Spade Queen, FaceDown Diamond Four, FaceDown Heart Five
                  , FaceDown Club Eight, FaceDown Diamond Nine, FaceDown Heart Seven, FaceDown Heart Eight, FaceDown Heart Ten, FaceDown Heart Jack, FaceDown Heart Queen
                  , FaceDown Diamond Three, FaceDown Diamond Seven, FaceDown Club Five, FaceDown Diamond Jack, FaceDown Heart Nine
                  ]

testTalon = Cards [FaceUp Spade Three, FaceUp Heart Six, FaceUp Club Ten]

testTableau = Tableau $ M.fromList [(Sum 1, pile1), (Sum 2, pile2), (Sum 3, pile3), (Sum 4, pile4), (Sum 5, pile5), (Sum 6, pile6), (Sum 7, pile7)]
testFoundations = Foundations $ M.fromList [(Club, foundation1), (Spade, foundation2), (Heart, foundation3), (Diamond, foundation4)]

testGame = Game { gameTableau     = testTableau
                , gameStock       = testStock
                , gameTalon       = testTalon
                , gameFoundations = testFoundations }

main :: IO ()
main = hspec $ do
  describe "initialize new game" $ do

    it "should create a stock of 52 cards" $ do
      let (Cards s) = makeStock
      length s `shouldBe` 52

    it "should create a stock of 52 unique cards" $ do 
      let (Cards s) = makeStock
      length s `shouldBe` length (nub s)

    it "should create a suffled stock of 52 unique cards" $ do 
      (Cards s) <- shuffleStock makeStock
      length s `shouldBe` length (nub s)

    it "should create a unique shuffled stock each time" $ do 
      let stocks' = take 100 $ repeat $ shuffleStock makeStock
          stocks = 
            foldr go (return []) stocks'
            where go x xs = do
                    x'  <- x
                    xs' <- xs
                    return $ x':xs'
      result <- (==) <$> (length <$> stocks) <*> ((length . nub) <$> stocks)
      result `shouldBe` True

    it "should create a new Tableau of seven piles with 1 to 7 cards" $ do 
      let (Tableau piles') = dealTableau makeStock
          piles = map (\x -> snd x) $ M.toList piles'
      foldr (\(Cards p) (a, _) -> (a - 1, length p == a)) (7, False) piles `shouldBe` (0, True)
      
    it "should create a new game of Solitaire with 52 unique cards" $ do 
      game <- startGame
      let (Tableau piles')  = gameTableau game
          piles    = map (\x -> snd x) $ M.toList piles' 
          stock    = gameStock game
          allCards = (<> stock) $ foldr (\pile cs -> pile <> cs) (Cards []) piles
          (Cards result) = (==) <$> (length <$> allCards) <*> ((length . nub) <$> allCards)
      result `shouldBe` True 

  describe "transfer cards between card stacks" $ do

    it "should take cards from a pile" $ do 
      let (Cards result) = takeCard pileA
      length result `shouldBe` 9

    it "should add cards to a pile" $ do 
      let (Cards result) = addCard pileA pileB 
      length result `shouldBe` 11

    it "should determine if a card is a different color and the successor of another card" $ do 
      let cardA = (FaceUp Club Queen)
          cardB = (FaceUp Heart King)
          cardC = (FaceUp Club King) 
          cardD = (FaceUp Heart Jack) 
          result1 = isSuccessor cardA cardB
          result2 = isSuccessor cardA cardC 
          result3 = isSuccessor cardA cardD
          result4 = isSuccessor cardC Bottom
          result5 = isSuccessor Bottom cardC
      result1 `shouldBe` True
      result2 `shouldBe` False 
      result3 `shouldBe` False
      result4 `shouldBe` True 
      result5 `shouldBe` False

    it "should determine if a card is the same color and the predecessor of another card" $ do 
      let cardA = (FaceUp Club Two)
          cardB = (FaceUp Club Ace)
          cardC = (FaceUp Heart Ace)
          cardD = (FaceUp Club Three) 
          result1 = isPredecessor cardA cardB
          result2 = isPredecessor cardA cardC 
          result3 = isPredecessor cardA cardD
          result4 = isPredecessor cardB Bottom 
          result5 = isPredecessor Bottom cardB
      result1 `shouldBe` True
      result2 `shouldBe` False 
      result3 `shouldBe` False
      result4 `shouldBe` True 
      result5 `shouldBe` False

    it "should only take cards from a list if they are Face Up" $ do 
      let cards  = [FaceUp Club Ace, FaceUp Heart Two, FaceUp Spade Three, FaceUp Diamond Four, FaceUp Club Five, FaceDown Club Six]
          result = takeFaceUp cards
          go  = filter (== (FaceDown Club Six)) result
      length result `shouldBe` 5
      length go `shouldBe` 0

    it "should drop cards from a list if they are Face Up" $ do 
      let cards  = [FaceUp Club Ace, FaceUp Heart Two, FaceUp Spade Three, FaceUp Diamond Four, FaceDown Heart Five, FaceDown Club Six]
          result = dropFaceUp cards
          go = filter (\x -> x /= (FaceDown Club Six) && x /= (FaceDown Heart Five)) result
      length result `shouldBe` 2
      length go `shouldBe` 0

    it "should retrieve the last item in a list if there is one" $ do 
      let result1 = lastOrEmpty [FaceUp Diamond Six, FaceDown Diamond Seven, FaceDown Diamond Eight]
          result2 = lastOrEmpty []
      result1 `shouldBe` (FaceDown Diamond Eight)
      result2 `shouldBe` Bottom

    it "should retrieve the first item in a list if there is one" $ do 
      let result1 = firstOrEmpty [FaceUp Spade Jack, FaceDown Spade Queen, FaceDown Spade King]
          result2 = firstOrEmpty []
      result1 `shouldBe` (FaceUp Spade Jack)
      result2 `shouldBe` Bottom

    it "should determine if the first card in list B is the successor of the first card in list A" $ do
      let x = (Cards [FaceUp Spade Ace], Cards [FaceUp Heart Two])
          y = (Cards [FaceUp Spade Ace], Cards [FaceUp Spade Two])
          z = (Cards [FaceUp Spade Ace], Cards [FaceUp Heart Three])
          result1 = maybeSuccessor x
          result2 = maybeSuccessor y
          result3 = maybeSuccessor z
      result1 `shouldBe` (Just x)
      result2 `shouldBe` Nothing
      result3 `shouldBe` Nothing

    it "should determine if the first card in list B is the predecessor of the first card in list A" $ do 
      let x = (Cards [FaceUp Spade Two], Cards [FaceUp Spade Ace])
          y = (Cards [FaceUp Heart Two], Cards [FaceUp Spade Ace])
          z = (Cards [FaceUp Spade Ace], Cards [FaceUp Spade Two])
          result1 = maybePredecessor x
          result2 = maybePredecessor y
          result3 = maybePredecessor z
      result1 `shouldBe` (Just x)
      result2 `shouldBe` Nothing
      result2 `shouldBe` Nothing

    it "should determine if the first card in list B is the successor of the last item in sequence A" $ do 
      let x = (Cards [FaceUp Diamond Two, FaceUp Club Three, FaceUp Heart Four, FaceDown Heart King], Cards [FaceUp Spade Five])
          y = (Cards [FaceUp Diamond Two, FaceUp Club Three, FaceUp Heart Four, FaceDown Heart King], Cards [FaceUp Heart Five])
          result1 = maybeSuccessorOfSequence x
          result2 = maybeSuccessorOfSequence y
      result1 `shouldBe` (Just x)
      result2 `shouldBe` Nothing

    it "should take cards from one pile and add them to another" $ do 
      let ((Cards a), (Cards b)) = transferCard (pileA, pileB) 
      length a `shouldBe` 9
      length b `shouldBe` 11

    it "should transfer card A from one pile to another if card B is the successor" $ do
      let x = (Cards [FaceUp Spade Ace], Cards [FaceUp Heart Two])
          y = (Cards [], Cards [FaceUp Spade Ace, FaceUp Heart Two])
          result = transferCardToSuccessor x 
      result `shouldBe` (Just y)

    it "should transfer card A from one pile to another if card B is the predecessor" $ do
      let x = (Cards [FaceUp Spade Two], Cards [FaceUp Spade Ace])
          y = (Cards [], Cards [FaceUp Spade Two, FaceUp Spade Ace])
          result = transferCardToPredecessor x
      result `shouldBe` (Just y)

    it "should transfer cards if the first card in list B is the successor of the last item in sequence A" $ do 
      let x = (Cards [FaceUp Diamond Two, FaceUp Club Three, FaceUp Heart Four, FaceDown Heart King], Cards [FaceUp Spade Five])
          y = (Cards [FaceUp Heart King], Cards [FaceUp Diamond Two, FaceUp Club Three, FaceUp Heart Four, FaceUp Spade Five])
          result = transferSequenceToSuccessor x
      result `shouldBe` (Just y)

    it "should select the specified pile from the tableau" $ do 
      let (Tableau tableau) = dealTableau makeStock 
          (Cards result)    = selectCards 4 tableau
      length result `shouldBe` 4

    it "should transfer a card from the stock pile to the talon" $ do 
      g <- startGame
      let game      = transferCardFromStockToTalon g
          (Cards t) = gameTalon game 
          (Cards s) = gameStock game
      length t `shouldBe` 1
      length s `shouldBe` 23

    it "should transfer a card from the talon to the specified pile on the tableau" $ do 
      let game         = transferCardFromTalonToPile testGame 5
          t            = gameTalon game 
          (Tableau ps) = gameTableau game
          p            = selectCards 5 ps
      t `shouldBe` Cards [FaceUp Heart Six, FaceDown Club Ten]
      p `shouldBe` Cards [FaceUp Spade Three, FaceUp Heart Four, FaceUp Spade Five, FaceUp Diamond Six, FaceUp Spade Seven]

    it "should transfer cards from one pile to another" $ do 
      let game         = transferCardsFromPileToPile testGame 1 5
          (Tableau ps) = gameTableau game 
          p            = selectCards 1 ps 
          p'           = selectCards 5 ps 
      p `shouldBe` Cards [FaceUp Diamond King, FaceDown Diamond Eight]
      p' `shouldBe` Cards [FaceUp Club Ace, FaceUp Heart Two, FaceUp Club Three, FaceUp Heart Four, FaceUp Spade Five, FaceUp Diamond Six, FaceUp Spade Seven]

    it "should transfer a card from a pile to a foundation" $ do 
      let game = transferCardFromPileToFoundation testGame 6 Diamond
          (Tableau ps)     = gameTableau game 
          p                = selectCards 6 ps 
          (Foundations fs) = gameFoundations game
          (Just f) = M.lookup Diamond fs
      p `shouldBe` Cards [FaceUp Spade King, FaceDown Heart King]
      f `shouldBe` Cards [FaceUp Diamond Ace]

    it "should transfer a card from the talon to a foundation" $ do 
      let game             = transferCardFromTalonToFoundation testGame Spade
          t                = gameTalon game 
          (Foundations fs) = gameFoundations game 
          Just f           = M.lookup Spade fs
      t `shouldBe` Cards [FaceUp Heart Six, FaceUp Club Ten]
      f `shouldBe` Cards [FaceUp Spade Three, FaceUp Spade Two, FaceUp Spade Ace]

    