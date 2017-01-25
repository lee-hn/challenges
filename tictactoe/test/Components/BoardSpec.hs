module Components.BoardSpec where

import Test.Hspec
import Components.Board

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board0x0 = Rows 0
  let board2x2 = Rows 2
  let board3x3 = Rows 3

  describe "boardDimension" $ do
    it "returns 0 for a 0x0 board" $ do
      shouldBe (boardDimension board0x0) 0

    it "returns 2 for a 2x2 board" $ do
      shouldBe (boardDimension board2x2) 2

    it "returns 3 for a 3x3 board" $ do
      shouldBe (boardDimension board3x3) 3

  describe "boardSpaces" $ do
    it "returns an empty list for a 0x0 board" $ do
      shouldBe (boardSpaces board0x0) []

    it "returns a list from 0 to 3 for a 2x2 board" $ do
      shouldBe (boardSpaces board2x2) [0..3]

    it "returns a list from 0 to 8 for a 3x3 board" $ do
      shouldBe (boardSpaces board3x3) [0..8]

  describe "boardRows" $ do
    it "returns an empty list for a 0x0 board" $ do
      shouldBe (boardRows board0x0) [[]]

    it "returns the rows for a 2x2 board" $ do
      let rows = [[0, 1], [2, 3],
                  [0, 2], [1, 3],
                  [0, 3], [1, 2]]
      shouldBe (boardRows board2x2) rows

    it "returns the rows for a 3x3 board" $ do
      let rows = [[0, 1, 2], [3, 4, 5], [6, 7, 8],
                  [0, 3, 6], [1, 4, 7], [2, 5, 8],
                  [0, 4, 8], [2, 4, 6]]
      shouldBe (boardRows board3x3) rows
