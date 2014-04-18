import TwentyFortyEight
import Test.Hspec
import qualified Data.Map as Map


start = (0,0)
end = (3,0)
lineAtFront = setCoords empty [((0,j), Just 2) | j <- [0 .. top]]
lineAtEnd   = setCoords empty [((3,j), Just 2) | j <- [0 .. top]]
lineAtSecond   = setCoords empty [((1,j), Just 2) | j <- [0 .. top]]
--Made the subsequent indexes different to observe whether the
--shifts preserve order
twoRowsAtFront = setCoords empty [((i,j), Just (2 * i)) | i <- [0,1], j <- [0 .. top]]
twoRowsAtSecond = setCoords empty [((i,j), Just (2 * (i - 1))) | i <- [1,2], j <- [0 .. top]] 
twoRowsAtEnd = setCoords empty [((i,j), Just (2 * (i - 2))) | i <- [2,3], j <- [0 .. top]] 
atStart = set empty (start, Just 2)
atEnd   = set empty ((3,0), Just 2)
atSecondPosition = set empty ((1,0), Just 2)
twoInRow = set atStart ((1,0), Just 2)
initial = set empty (start, Just 2)
afterShift = set empty ((3,0), Just 2)
mergedTwoInRow = set empty ((1,0), four)
fullOfTwos = setCoords empty [((i, j), two) | i <- [0 .. top], j <- [0 .. top]]
twoFourLines = setCoords empty [((i,j), four) | i <- [1, 3], j <- [0 .. top]]
twoFourLinesAtEnd = setCoords empty [((i,j), four) | i <- [2, 3], j <- [0 .. top]]
lineAtTop = setCoords empty [((i,0), two) | i <- [0 .. top]]

shifting = do
  describe "Shifting board to the right" $ do
    describe "Shifting one unit to the right" $ do
      describe "canShift" $ do
          it "Is true when there are no blocks to the right" $ do
            canShiftSquare atStart start`shouldBe` True
          it "Is false when at the end" $ do
            canShiftSquare atEnd (3,0) `shouldBe` False
          it "Is false when a square follows immediately to the right" $ do
            canShiftSquare twoInRow start `shouldBe` False
      describe "canShiftSquare" $ do
        it "Shifts one unit to the right when empty" $ do
          shiftSquare atStart start `shouldBe` atSecondPosition
        it "Does not shift the end square" $ do 
          shiftSquare atEnd (3,0) `shouldBe` atEnd
        it "Does not shift if it means overriding a block" $ do
          shiftSquare twoInRow start `shouldBe` twoInRow
    describe "board level shifts" $ do
      describe "Single shift" $ do
        it "shifts shift in front to second" $ do
          shift lineAtFront `shouldBe` lineAtSecond
        it "does not change those at end" $ do
          shift lineAtEnd `shouldBe` lineAtEnd
        it "shifts two adjacent rows one space to the left" $ do
          shift twoRowsAtFront `shouldBe` twoRowsAtSecond
      describe "when shifting entire board to the right" $ do
        it "shifts anything in the first position to the end" $ do
          fullShift initial `shouldBe` afterShift
        it "shifts an entire row to the end" $ do
          fullShift lineAtFront `shouldBe` lineAtEnd
        it "shifts of the two adjacent rows at different starting indexes have same result" $ do
          fullShift twoRowsAtFront `shouldBe` fullShift twoRowsAtSecond
        it "shifts the first row and makes it look like the one at the end" $ do
          fullShift twoRowsAtFront `shouldBe` twoRowsAtEnd 
        it "shifts the two rows in starting at second x index and returns board where all rows are at end" $ do
          fullShift twoRowsAtSecond `shouldBe` twoRowsAtEnd

merging = do
  describe "Merging" $ do
    describe "canMerge" $ do
      it "with equal squares is true" $ do
        canMergeSquare twoInRow start `shouldBe` True
      it "with empty and full square is false" $ do
        canMergeSquare atStart start `shouldBe` False
      it "with two empty blocks is false" $ do
        canMergeSquare empty start `shouldBe` False
      it "with two adjacent unequal blocks cannot be merged" $ do
        canMergeSquare (set twoInRow (start, Just 4)) start `shouldBe` False 
    describe "mergeSquare" $ do
      it "merges equal squares into one square with double value of origional and empty adjacent squares" $ do
        mergeSquare twoInRow start `shouldBe` mergedTwoInRow
      it "does not merge non-equal squares leaving the board the same" $ do
        mergeSquare twoRowsAtFront start `shouldBe` twoRowsAtFront 
      it "does not break when merging nothing" $ do
        mergeSquare empty start `shouldBe` empty
      it "does not merge with empty squares" $ do
        mergeSquare atStart start `shouldBe` atStart
      it "does not merge square at end" $ do 
        mergeSquare atEnd end `shouldBe` atEnd
    describe "merge" $ do
      it "merges a board full of twos into one that contains two line of fours at indexes 1, 3" $ do
        merge fullOfTwos `shouldBe` twoFourLines

fullTransition = do
    describe "fullLeftTransision" $ do
      it "merges a board full of twos to one with two rows of fours at the end" $ do
        transition fullOfTwos `shouldBe` twoFourLinesAtEnd

rotation = do
    describe "rotation" $ do
      describe "rotation of single points" $ do
        it "rotates (0,0) to (3,0)" $ do
          rotateCoord (0,0) `shouldBe` (3,0)
        it "when rotated 4 times, returns the same value" $ do
          (rotateCoord . rotateCoord . rotateCoord . rotateCoord) (0,0) `shouldBe` (0,0)
        it "rotates (1,2) to (1,1)" $ do
          rotateCoord (1,2) `shouldBe` (1,1)
        it "rotates (3,0) to (3,3)" $ do
          rotateCoord (3,0) `shouldBe` (3,3)
        it "when rotated twice rotates (3,0) to (0,3)" $ do
          (rotateCoord . rotateCoord) (3,0) `shouldBe` (0,3)
      describe "rotation of entire board" $ do
        it "when rotating an empty board returns an empty board" $ do
          rotate empty `shouldBe` empty
        it "squares on the end become squares on the top of the board" $ do
          rotate lineAtEnd `shouldBe` lineAtTop
        it "when rotated for times returns the same board" $ do
          (rotate . rotate . rotate . rotate) twoFourLinesAtEnd `shouldBe` twoFourLinesAtEnd

main = hspec $ do
  shifting
  merging
  fullTransition
  rotation
