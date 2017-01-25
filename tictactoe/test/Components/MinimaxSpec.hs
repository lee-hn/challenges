module Components.MinimaxSpec where

import Test.Hspec
import Components.Minimax
import Components.Rules ( GameData(Components) )
import Components.Board ( BoardData(Rows) )
import Components.Moves ( MovesData(GameState) )

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
      let game = Components board almostWinGame
      shouldBe (scoreMove 2 game) 1

    it "scores 0 if the move results in a draw" $ do
      let game = Components board almostDrawGame
      shouldBe (scoreMove 8 game) 0

    it "scores -1 if the move results in a loss" $ do
      let game = Components board almostLossGame
      shouldBe (scoreMove 8 game) (-1)

  describe "possibleMoves" $ do
    it "returns empty list if the game is won" $ do
      let game = Components board winGame
      shouldBe (possibleMoves game) []

    it "returns empty list if the game is draw" $ do
      let game = Components board drawGame
      shouldBe (possibleMoves game) []

    it "returns all spaces if no moves have been played" $ do
      let game = Components board newGame
      shouldBe (possibleMoves game) [0..8]

    it "returns empty spaces in if some moves have been played" $ do
      let game = Components board (GameState [1, 3, 5, 7])
      shouldBe (possibleMoves game) [0, 2, 4, 6, 8]

  describe "scorePossibleMoves" $ do
    it "returns scores for one possible move left to play" $ do
      let game = Components board almostWinGame
      shouldBe (scorePossibleMoves game) [(2, 1)]

    it "returns scores for two possible moves left to play" $ do
      let game = Components board almostLossGame
      shouldBe (scorePossibleMoves game) [(6, 1), (8, -1)]

  describe "chooseBestMove" $ do
    it "chooses best move when one move has max score" $ do
      let moveScores = [(6, -1), (7, 0), (8, 1)]
      shouldBe (chooseBestMove moveScores) (8, 1)

    it "chooses one of the best moves when multiple moves have max score" $ do
      let moveScores = [(6, -1), (7, 1), (8, 1)]
      shouldBe (elem (chooseBestMove moveScores) [(7, 1), (8, 1)]) True
