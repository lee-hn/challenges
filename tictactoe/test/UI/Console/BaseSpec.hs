{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module UI.Console.BaseSpec where

import Test.Hspec
import UI.Console.Base
import UI.UI ( MonadUI )
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [''MonadUI]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let writeFixture = def {
      _writeLine = \line -> tell line
  }

  let readFixture = def {
      _readLine = return "input"
  }

  let ioFixture = def {
      _writeLine = \line -> tell line
    , _readLine = return "response"
  }

  describe "displayMessage" $ do
    it "sends string to output" $ do
      let output = logTestFixture (displayMessage "message") writeFixture
      shouldBe output "message\n"

  describe "getUserInput" $ do
    it "receives input string" $ do
      let input = unTestFixture getUserInput readFixture
      shouldBe input "input"

  describe "displayPrompt" $ do
    it "sends prompt to output" $ do
      let (input, output) = evalTestFixture (displayPrompt "prompt") ioFixture
      shouldBe output "prompt\n"
      shouldBe input "response"
