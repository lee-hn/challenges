{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module GameSpec where

import Test.Hspec
import Game
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )
import Rules
    (
      GameData(Game)
    , OutcomeData(PlayerOneWin, PlayerTwoWin, Draw)
    )
import Settings ( PlayerData(Players) )
import Mocks ( MockPlayerData(MockPlayer) )
import UI ( MonadUI )
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [''MonadUI]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let newGame = GameState []
  let finishedGame = GameState [4, 2, 5, 1, 6, 0]
  let unfinishedGame = GameState [4, 2, 5, 1, 6]
  let players = Players MockPlayer MockPlayer

  let fixture = def {
      _writeLine = \line -> tell line
  }

  describe "endGame" $ do
    it "gives result if Player One wins" $ do
      let result = logTestFixture (endGame PlayerOneWin) fixture
      shouldBe result "Player One wins\n"

    it "gives result if Player Two wins" $ do
      let result = logTestFixture (endGame PlayerTwoWin) fixture
      shouldBe result "Player Two wins\n"

    it "gives result if draw" $ do
      let result = logTestFixture (endGame Draw) fixture
      shouldBe result "Game ends in a draw\n"

{-
  describe "nextMove" $ do
    it "returns a move when next player is player one" $ do
      let game = Game board newGame
      shouldBe (nextMove game players) 0

    it "returns a move when next player is player two" $ do
      let game = Game board unfinishedGame
      shouldBe (nextMove game players) 0
-}

  describe "runGame" $ do
    it "ends game if the game is finished" $ do
      let game = Game board finishedGame
      let result = logTestFixture (runGame game players) fixture
      shouldBe result "Player Two wins\n"

    it "makes move and ends game if game is unfinished" $ do
      let game = Game board unfinishedGame
      let gameString = " O | O | O \n\
                       \---+---+---\n\
                       \ 3 | X | X \n\
                       \---+---+---\n\
                       \ X | 7 | 8 "
      let result = logTestFixture (runGame game players) fixture
      shouldBe result ("\nTurn 6:\n" ++ gameString ++ "\nPlayer Two wins\n")
