{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Players.ComputerSpec where

import Test.Hspec
import Players.Computer
import Components.Rules ( GameData(Game) )
import Components.Board ( BoardData(Rows) )
import Components.Moves ( MovesData(GameState) )
import UI.UI ( MonadUI )
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [''MonadUI]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let computer = ComputerPlayer
  let newGame = GameState []
  let nearEndGame = GameState [4, 0, 1, 7, 5, 3, 6, 8]
  let midGame = GameState [4, 0, 1, 7, 3, 6]

  let fixture = def {
      _readLine = return ""
  }

  describe "makeMove" $ do
    it "makes the only possible move" $ do
      let game = Game board nearEndGame
      let move = unTestFixture (makeMove computer game) fixture
      shouldBe move 2

    it "chooses the winning move out of three possible moves" $ do
      let game = Game board midGame
      let move = unTestFixture (makeMove computer game) fixture
      shouldBe move 5

    it "can make a move at the beginning of a game" $ do
      let game = Game board newGame
      let move = unTestFixture (makeMove computer game) fixture
      shouldBe (elem move [0..8]) True
