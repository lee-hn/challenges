module GameSpec where

import Test.Hspec
import Game
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )
import Rules
    ( GameData(Game)
    , OutcomeData(PlayerOneWin, PlayerTwoWin, Draw)
    )
import Settings ( PlayerData(Players) )
import Player ( PlayerTypeData(Mock) )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let newGame = GameState []
  let finishedGame = GameState [4, 2, 5, 1, 6, 0]
  let unfinishedGame = GameState [4, 2, 5, 1, 6]
  let players = Players Mock Mock

  describe "endGame" $ do
    it "gives result if Player One wins" $ do
      shouldBe (endGame PlayerOneWin) "Player One wins"

    it "gives result if Player Two wins" $ do
      shouldBe (endGame PlayerTwoWin) "Player Two wins"

    it "gives result if draw" $ do
      shouldBe (endGame Draw) "Draw"

  describe "nextMove" $ do
    it "returns a move when next player is player one" $ do
      let game = Game board newGame
      shouldBe (nextMove game players) 0

    it "returns a move when next player is player two" $ do
      let game = Game board unfinishedGame
      shouldBe (nextMove game players) 0

  describe "runGame" $ do
    it "returns result if the game is finished" $ do
      let game = Game board finishedGame
      shouldBe (runGame game players) "Player Two wins"

    it "makes move and returns result if game is finished" $ do
      let game = Game board unfinishedGame
      shouldBe (runGame game players) "Player Two wins"
