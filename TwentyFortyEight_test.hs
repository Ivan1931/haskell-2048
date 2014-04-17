import TwentyFortyEight
import Test.Hspec
import qualified Data.Map as Map

empty = fromString "****\n\
                   \****\n\
                   \****\n\
                   \****"

main = hspec $ do
  describe "Shifting board to the right" $ do
    describe "Shifting one unit to the right" $ do
      let atStart = set empty ((0,0), Just 2)
          atEnd   = set empty ((3,0), Just 2)
          atSecondPosition = set empty ((1,0), Just 2)
          twoInRow = set atStart ((1,0), Just 2)

      describe "canShift" $ do
          it "Is true when there are no blocks to the right" $ do
            canShiftSquare atStart (0,0)`shouldBe` True

          it "Is false when at the end" $ do
            canShiftSquare atEnd (3,0) `shouldBe` False

          it "Is false when a square follows immediately to the right" $ do
            canShiftSquare twoInRow (0,0) `shouldBe` False

      describe "canShiftSquare" $ do
        it "Shifts one unit to the right when empty" $ do
          shiftSquare atStart (0,0) `shouldBe` atSecondPosition

        it "Does not shift the end square" $ do 
          shiftSquare atEnd (3,0) `shouldBe` atEnd
        
        it "Does not shift if it means overriding a block" $ do
          shiftSquare twoInRow (0,0) `shouldBe` twoInRow
      
    describe "board level shifts" $ do
      let lineAtFront = setCoords empty [((0,j), Just 2) | j <- [0 .. top]]
          lineAtEnd   = setCoords empty [((3,j), Just 2) | j <- [0 .. top]]
          lineAtSecond   = setCoords empty [((1,j), Just 2) | j <- [0 .. top]]
          --Made the subsequent indexes different to observe whether the
          --shifts preserve order
          twoInRowAtFront = setCoords empty [((i,j), Just (2 * i)) | i <- [0,1], j <- [0 .. top]]
          twoInRowAtSecond = setCoords empty [((i,j), Just (2 * (i - 1))) | i <- [1,2], j <- [0 .. top]] 

      describe "Single shiftColumn" $ do
        it "shifts those in front to second" $ do
          shiftColumn lineAtFront `shouldBe` lineAtSecond
        it "does not change those at end" $ do
          shiftColumn lineAtEnd `shouldBe` lineAtEnd
        it "shifts two adjacent rows one space to the left" $ do
          shiftColumn twoInRowAtFront `shouldBe` twoInRowAtSecond

      describe "when shifting entire board to the right" $ do
        let initial = set empty ((0,0), Just 2)
            afterShift = set empty ((3,0), Just 2)
        it "shifts anything in the first position to the end" $ do
          fullShift initial `shouldBe` afterShift
        
        it "shifts an entire row to the end" $ do
          fullShift lineAtFront `shouldBe` lineAtEnd

