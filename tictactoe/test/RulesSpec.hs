module RulesSpec where

import Test.Hspec
import Rules
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let newGame = GameState []
  let playerOneWinGame = GameState [4, 0, 1, 7, 5, 3, 6, 8, 2]
  let playerTwoWinGame = GameState [4, 0, 5, 1, 6, 2]
  let drawGame = GameState [4, 0, 1, 7, 5, 3, 6, 2, 8]

  describe "isSpace" $ do
    it "returns True if move is in a board space" $ do
      shouldBe (isSpace 0 board) True

    it "returns False if move is not in a board space" $ do
      shouldBe (isSpace 9 board) False

  describe "isUnoccupied" $ do
    it "returns True if move is in unoccupied space" $ do
      shouldBe (isUnoccupied 4 newGame) True

    it "returns False if move is in occupied space" $ do
      shouldBe (isUnoccupied 4 (GameState [4])) False

  describe "isWin" $ do
    it "returns False if neither player has won" $ do
      let game = Game board newGame
      shouldBe (isWin game) False

    it "returns True if a player has won" $ do
      let game = Game board playerTwoWinGame
      shouldBe (isWin game) True

  describe "isDraw" $ do
    it "returns False if board is not full" $ do
      let game = Game board newGame
      shouldBe (isDraw game) False

    it "returns False if board is full and a player has won" $ do
      let game = Game board playerOneWinGame
      shouldBe (isDraw game) False

    it "returns True is board is full and no player has won" $ do
      let game = Game board drawGame
      shouldBe (isDraw game) True

  describe "outcome" $ do
    it "returns PlayerOneWin if there is a win by player one" $ do
      let game = Game board playerOneWinGame
      shouldBe (outcome game) PlayerOneWin

    it "returns PlayerTwoWin if there is a win by player two" $ do
      let game = Game board playerTwoWinGame
      shouldBe (outcome game) PlayerTwoWin

    it "returns Draw if there is a draw" $ do
      let game = Game board drawGame
      shouldBe (outcome game) Draw

    it "returns Continue if there is no win or draw" $ do
      let game = Game board newGame
      shouldBe (outcome game) Continue

  describe "nextPlayer" $ do
    it "returns 0 if no one has played" $ do
      shouldBe (nextPlayer newGame) 0

    it "returns 1 if last player was player one" $ do
      shouldBe (nextPlayer (GameState [0])) 1

    it "returns 0 if last player was player two" $ do
      shouldBe (nextPlayer (GameState [0, 1])) 0
