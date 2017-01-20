module MinimaxSpec where

import Test.Hspec
import Minimax
import Rules ( GameData(Game) )
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let newGame = GameState []
  let almostWinGame = GameState [4, 0, 1, 7, 5, 3, 6, 8]
  let almostDrawGame = GameState [4, 0, 1, 7, 5, 3, 6, 2]
  let almostLossGame = GameState [4, 0, 1, 7, 5, 3, 2]
  let winGame = GameState [4, 0, 1, 7, 5, 3, 6, 8, 2]
  let drawGame = GameState [4, 0, 1, 7, 5, 3, 6, 2, 8]

  describe "scoreMove" $ do
    it "scores 1 if the move results in a win" $ do
      let game = Game board almostWinGame
      shouldBe (scoreMove 2 game) 1

    it "scores 0 if the move results in a draw" $ do
      let game = Game board almostDrawGame
      shouldBe (scoreMove 8 game) 0 

    it "scores -1 if the move results in a loss" $ do
      let game = Game board almostLossGame
      shouldBe (scoreMove 8 game) (-1)

  describe "possibleMoves" $ do
    it "returns empty list if the game is won" $ do
      let game = Game board winGame
      shouldBe (possibleMoves game) []

    it "returns empty list if the game is draw" $ do
      let game = Game board drawGame
      shouldBe (possibleMoves game) []

    it "returns all spaces if no moves have been played" $ do
      let game = Game board newGame
      shouldBe (possibleMoves game) [0..8]

    it "returns empty spaces if some moves have been played" $ do
      let game = Game board (GameState [1, 3, 5, 7])
      shouldBe (possibleMoves game) [0, 2, 4, 6, 8]

  describe "scorePossibleMoves" $ do
    it "returns list with score for one space left to play" $ do
      let game = Game board almostWinGame
      shouldBe (scorePossibleMoves game) [1]

    it "returns list with scores for two spaces left to play" $ do
      let game = Game board almostLossGame
      shouldBe (scorePossibleMoves game) [1, -1]

