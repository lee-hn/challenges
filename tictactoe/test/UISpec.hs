{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module UISpec where

import Test.Hspec
import UI
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )
import Rules ( GameData(Game) )
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [''MonadUI]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let newGame = GameState []
  let midGame = GameState [1, 3, 5, 7]

  describe "displayMessage" $ do
    it "sends string to output" $ do
      let fixture = def {
           _writeLine = \line -> tell line
          }
      let output = logTestFixture (displayMessage "message") fixture
      shouldBe output "message"

  describe "getUserInput" $ do
    it "receives input string" $ do
      let fixture = def {
            _readLine = return "input"
          }
      let input = unTestFixture getUserInput fixture
      shouldBe input "input"

  describe "displayPrompt" $ do
    it "sends prompt to output" $ do
      let fixture = def {
          _writeLine = \line -> tell line
        , _readLine = return "response"
        }
      let (input, output) = evalTestFixture (displayPrompt "prompt") fixture
      shouldBe output "prompt"
      shouldBe input "response"

  describe "labelSpace" $ do
    it "represents unplayed space as its index" $ do
      let label = " 0 "
      let game = Game board midGame
      shouldBe (labelSpace 0 game) label

    it "represents move by player one with marker X" $ do
      let label = " X "
      let game = Game board midGame
      shouldBe (labelSpace 1 game) label

    it "represents move by player two with marker O" $ do
      let label = " O "
      let game = Game board midGame
      shouldBe (labelSpace 3 game) label

  describe "convertGameToString" $ do
    it "represents a new game on 3x3 board as string" $ do
      let gameString = " 0 | 1 | 2 \n\
                       \---+---+---\n\
                       \ 3 | 4 | 5 \n\
                       \---+---+---\n\
                       \ 6 | 7 | 8 "
      let game = Game board newGame
      shouldBe (convertGameToString game) gameString

    it "represents in-progress game on 3x3 board as string" $ do
      let gameString = " 0 | X | 2 \n\
                       \---+---+---\n\
                       \ O | 4 | X \n\
                       \---+---+---\n\
                       \ 6 | O | 8 "
      let game = Game board midGame
      shouldBe (convertGameToString game) gameString

  describe "displayGame" $ do
    it "displays string representation of a game" $ do
      let gameString = " 0 | 1 | 2 \n\
                       \---+---+---\n\
                       \ 3 | 4 | 5 \n\
                       \---+---+---\n\
                       \ 6 | 7 | 8 "
      let game = Game board newGame
      let fixture = def {
          _writeLine = \line -> tell line
        }
      let output = logTestFixture (displayGame game) fixture
      shouldBe output gameString
