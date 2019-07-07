module Spec where

import           Data.List
import qualified Data.Map as M
import           Data.Monoid
import           Lib
import           Test.Hspec

deck  = Card <$> [Spade .. Diamond] <*> [Ace .. King]
pileA = Cards . (take 10) $ deck
pileB = Cards . (take 10) . reverse $ deck

pile1 = Cards [Card Club Ace, Card Heart Two, Card Club Three, Card Diamond King, Card Diamond Eight]
pile2 = Cards [Card Spade Jack, Card Spade Nine, Card Diamond Queen, Card Heart Three]
pile3 = Cards [Card Spade Eight, Card Club Two, Card Club Jack]
pile4 = Cards []
pile5 = Cards [Card Heart Four, Card Spade Five, Card Diamond Six, Card Spade Seven]
pile6 = Cards [Card Diamond Ace, Card Spade King, Card Heart King]
pile7 = Cards [Card Diamond Two, Card Club King, Card Heart Ace, Card Diamond Ten]

foundation1 = Cards []
foundation2 = Cards [Card Spade Two, Card Spade Ace]
foundation3 = Cards []
foundation4 = Cards []

testStock = Cards [ Card Club Four, Card Diamond Five, Card Spade Two, Card Club Six, Card Club Seven, Card Club Nine
                  , Card Club Queen, Card Spade Four, Card Spade Six, Card Spade Ten, Card Spade Queen, Card Diamond Four, Card Heart Five
                  , Card Club Eight, Card Diamond Nine, Card Heart Seven, Card Heart Eight, Card Heart Ten, Card Heart Jack, Card Heart Queen
                  , Card Diamond Three, Card Diamond Seven, Card Club Five, Card Diamond Jack, Card Heart Nine
                  ]

testTalon = Cards [Card Spade Three, Card Heart Six, Card Club Ten]

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
      let cardA = (Card Club Queen)
          cardB = (Card Heart King)
          cardC = (Card Club King) 
          cardD = (Card Heart Jack) 
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
      let cardA = (Card Club Two)
          cardB = (Card Club Ace)
          cardC = (Card Heart Ace)
          cardD = (Card Club Three) 
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

    it "should only take cards from a list if they are sequential" $ do 
      let cards  = [Card Club Ace, Card Heart Two, Card Spade Three, Card Diamond Four, Card Club Five, Card Club Six]
          result = takeSequence cards
          go  = filter (== (Card Club Six)) result
      length result `shouldBe` 5
      length go `shouldBe` 0

    it "should drop cards from a list if they are sequential" $ do 
      let cards  = [Card Club Ace, Card Heart Two, Card Spade Three, Card Diamond Four, Card Heart Five, Card Club Six]
          result = dropSequence cards
          go = filter (\x -> x /= (Card Club Six) && x /= (Card Heart Five)) result
      length result `shouldBe` 2
      length go `shouldBe` 0

    it "should retrieve the last item in a list if there is one" $ do 
      let result1 = lastOrEmpty [Card Diamond Six, Card Diamond Seven, Card Diamond Eight]
          result2 = lastOrEmpty []
      result1 `shouldBe` (Card Diamond Eight)
      result2 `shouldBe` Bottom

    it "should retrieve the first item in a list if there is one" $ do 
      let result1 = firstOrEmpty [Card Spade Jack, Card Spade Queen, Card Spade King]
          result2 = firstOrEmpty []
      result1 `shouldBe` (Card Spade Jack)
      result2 `shouldBe` Bottom

    it "should determine if the first card in list B is the successor of the first card in list A" $ do
      let x = (Cards [Card Spade Ace], Cards [Card Heart Two])
          y = (Cards [Card Spade Ace], Cards [Card Spade Two])
          z = (Cards [Card Spade Ace], Cards [Card Heart Three])
          result1 = maybeSuccessor x
          result2 = maybeSuccessor y
          result3 = maybeSuccessor z
      result1 `shouldBe` (Just x)
      result2 `shouldBe` Nothing
      result3 `shouldBe` Nothing

    it "should determine if the first card in list B is the predecessor of the first card in list A" $ do 
      let x = (Cards [Card Spade Two], Cards [Card Spade Ace])
          y = (Cards [Card Heart Two], Cards [Card Spade Ace])
          z = (Cards [Card Spade Ace], Cards [Card Spade Two])
          result1 = maybePredecessor x
          result2 = maybePredecessor y
          result3 = maybePredecessor z
      result1 `shouldBe` (Just x)
      result2 `shouldBe` Nothing
      result2 `shouldBe` Nothing

    it "should determine if the first card in list B is the successor of the last item in sequence A" $ do 
      let x = (Cards [Card Diamond Two, Card Club Three, Card Heart Four, Card Heart King], Cards [Card Spade Five])
          y = (Cards [Card Diamond Two, Card Club Three, Card Heart Four, Card Heart King], Cards [Card Heart Five])
          result1 = maybeSuccessorOfSequence x
          result2 = maybeSuccessorOfSequence y
      result1 `shouldBe` (Just x)
      result2 `shouldBe` Nothing

    it "should take cards from one pile and add them to another" $ do 
      let ((Cards a), (Cards b)) = transferCard (pileA, pileB) 
      length a `shouldBe` 9
      length b `shouldBe` 11

    it "should transfer card A from one pile to another if card B is the successor" $ do
      let x = (Cards [Card Spade Ace], Cards [Card Heart Two])
          y = (Cards [], Cards [Card Spade Ace, Card Heart Two])
          result = transferCardToSuccessor x 
      result `shouldBe` (Just y)

    it "should transfer card A from one pile to another if card B is the predecessor" $ do
      let x = (Cards [Card Spade Two], Cards [Card Spade Ace])
          y = (Cards [], Cards [Card Spade Two, Card Spade Ace])
          result = transferCardToPredecessor x
      result `shouldBe` (Just y)

    it "should transfer cards if the first card in list B is the successor of the last item in sequence A" $ do 
      let x = (Cards [Card Diamond Two, Card Club Three, Card Heart Four, Card Heart King], Cards [Card Spade Five])
          y = (Cards [Card Heart King], Cards [Card Diamond Two, Card Club Three, Card Heart Four, Card Spade Five])
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
      t `shouldBe` Cards [Card Heart Six, Card Club Ten]
      p `shouldBe` Cards [Card Spade Three, Card Heart Four, Card Spade Five, Card Diamond Six, Card Spade Seven]

    it "should transfer cards from one pile to another" $ do 
      let game         = transferCardsFromPileToPile testGame 1 5
          (Tableau ps) = gameTableau game 
          p            = selectCards 1 ps 
          p'           = selectCards 5 ps 
      p `shouldBe` Cards [Card Diamond King, Card Diamond Eight]
      p' `shouldBe` Cards [Card Club Ace, Card Heart Two, Card Club Three, Card Heart Four, Card Spade Five, Card Diamond Six, Card Spade Seven]

    it "should transfer a card from a pile to a foundation" $ do 
      let game = transferCardFromPileToFoundation testGame 6 Diamond
          (Tableau ps)     = gameTableau game 
          p                = selectCards 6 ps 
          (Foundations fs) = gameFoundations game
          (Just f) = M.lookup Diamond fs
      p `shouldBe` Cards [Card Spade King, Card Heart King]
      f `shouldBe` Cards [Card Diamond Ace]

    it "should transfer a card from the talon to a foundation" $ do 
      let game             = transferCardFromTalonToFoundation testGame Spade
          t                = gameTalon game 
          (Foundations fs) = gameFoundations game 
          Just f           = M.lookup Spade fs
      t `shouldBe` Cards [Card Heart Six, Card Club Ten]
      f `shouldBe` Cards [Card Spade Three, Card Spade Two, Card Spade Ace]

    