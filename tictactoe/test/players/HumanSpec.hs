{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Players.HumanSpec where

import Test.Hspec
import Players.Human
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

  let human = HumanPlayer
  let board = Rows 3
  let newGame = GameState []
  let midGame = GameState [1]

  let readFixture = def {
      _readLine = return "0"
  }

  let ioFixture = def {
      _writeLine = \line -> tell line
    , _readLine = return ""
  }

  describe "makeMove" $ do
    it "gets the move from user input" $ do
      let game = Game board newGame
      let move = unTestFixture (makeMove human game) readFixture
      shouldBe move 0

  describe "isValid" $ do
    it "returns False and displays error if user input is not a number" $ do
      let game = Game board newGame
      let (input, output) = evalTestFixture (isValid Nothing game) ioFixture
      shouldBe output "That is not a number\n"
      shouldBe input False

    it "returns False and displays error if user input is an occupied space" $ do
      let game = Game board midGame
      let (input, output) = evalTestFixture (isValid (Just 1) game) ioFixture
      shouldBe output "That space is already occupied\n"
      shouldBe input False


    it "returns False and displays error if user input is an occupied space" $ do
      let game = Game board newGame
      let (input, output) = evalTestFixture (isValid (Just 9) game) ioFixture
      shouldBe output "That space does not exist on the board\n"
      shouldBe input False

    it "returns True if user input is valid" $ do
      let game = Game board midGame
      let input = unTestFixture (isValid (Just 2) game) readFixture
      shouldBe input True

