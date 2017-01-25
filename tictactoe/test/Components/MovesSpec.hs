module Components.MovesSpec where

import Test.Hspec
import Components.Moves

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let newGame = GameState []

  describe "allMoves" $ do
    it "returns empty list if there are no allMoves" $ do
      shouldBe (allMoves newGame) []

    it "returns list of allMoves" $ do
      let gameState = GameState [4, 0]
      shouldBe (allMoves $ gameState) [4, 0]

  describe "lastMove" $ do
    it "returns Nothing if there are no allMoves" $ do
      shouldBe (lastMove newGame) Nothing

    it "returns the last move in the game state" $ do
      let gameState = GameState [4, 0]
      shouldBe (lastMove gameState) (Just 0)

  describe "addMove" $ do
    it "returns the game state with the newest move" $ do
      let gameState = GameState [3]
      shouldBe (addMove 3 newGame) gameState
    it "adds the new move to the end of the move list" $ do
      let oldGameState = GameState [3]
      let newGameState = GameState [3, 4]
      shouldBe (addMove 4 oldGameState) newGameState

  describe "lastPlayer" $ do
    it "returns Nothing if there are no moves" $ do
      shouldBe (lastPlayer newGame) Nothing

    it "returns 0 if there is one move" $ do
      let gameState = GameState [3]
      shouldBe (lastPlayer $ gameState) (Just 0)

    it "returns 1 if there is even number of moves" $ do
      let gameState = GameState [4, 0]
      shouldBe (lastPlayer $ gameState) (Just 1)

    it "returns 0 if there is odd number of moves" $ do
      let gameState = GameState [2, 0, 6]
      shouldBe (lastPlayer $ gameState) (Just 0)

  describe "playerMoves" $ do
    it "returns first player's allMoves when given 0" $ do
      let gameState = GameState [2, 4, 6, 8]
      shouldBe (playerMoves 0 gameState) [2, 6]

    it "returns second player's allMoves when given 1" $ do
      let gameState = GameState [2, 4, 6, 8]
      shouldBe (playerMoves 1 gameState) [4, 8]

    it "returns empty list when there are no allMoves" $ do
      shouldBe (playerMoves 0 newGame) []
