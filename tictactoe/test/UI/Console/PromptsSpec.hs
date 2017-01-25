{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module UI.Console.PromptsSpec where

import Test.Hspec
import UI.Console.Prompts
import UI.UI ( MonadUI )
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [''MonadUI]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "promptForMove" $ do
    it "sends a prompt for a move and receives move as an integer" $ do
      let fixture = def {
          _writeLine = \line -> tell line
        , _readLine = return "4"
      }
      let (input, output) = evalTestFixture promptForMove fixture
      shouldBe output "Please enter the number of the space where you want to make a move\n"
      shouldBe input (Just 4)

    it "returns Nothing if input move cannot be read as an integer" $ do
      let fixture = def {
          _writeLine = \line -> tell line
        , _readLine = return "not an integer"
      }
      let (input, output) = evalTestFixture promptForMove fixture
      shouldBe output "Please enter the number of the space where you want to make a move\n"
      shouldBe input Nothing

    it "returns Nothing if input move cannot be read as an integer" $ do
      let fixture = def {
          _writeLine = \line -> tell line
        , _readLine = return ""
      }
      let (input, output) = evalTestFixture promptForMove fixture
      shouldBe output "Please enter the number of the space where you want to make a move\n"
      shouldBe input Nothing

  describe "promptForPlayerOrder" $ do
    it "sends a prompt for player order and receives user input of 1" $ do
      let fixture = def {
          _writeLine = \line -> tell line
        , _readLine = return "1"
      }
      let (input, output) = evalTestFixture promptForPlayerOrder fixture
      shouldBe output "Do you want to play first or second? Please enter 1 or 2\n"
      shouldBe input 1

    it "sends a prompt for player order and receives user input of 2" $ do
      let fixture = def {
          _writeLine = \line -> tell line
        , _readLine = return "2"
      }
      let (input, output) = evalTestFixture promptForPlayerOrder fixture
      shouldBe output "Do you want to play first or second? Please enter 1 or 2\n"
      shouldBe input 2

