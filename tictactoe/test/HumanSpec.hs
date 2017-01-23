{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module HumanSpec where

import Test.Hspec
import Human
import Rules ( GameData(Game) )
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )
import UI ( MonadUI )
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [''MonadUI]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let human = HumanPlayer
  let board = Rows 3
  let newGame = GameState []
  let game = Game board newGame

  describe "makeMove" $ do
    it "gets the move from user input if valid" $ do
      let fixture = def {
          _readLine = return "0"
      }
      let move = unTestFixture (makeMove human game) fixture
      shouldBe move 0
