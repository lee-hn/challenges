{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module UISpec where

import Test.Hspec
import UI
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )
import Rules
    (
      GameData(Game)
    , OutcomeData(PlayerOneWin, PlayerTwoWin, Draw)
    )
import Errors ( ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError) )
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
      shouldBe output "message"

  describe "getUserInput" $ do
    it "receives input string" $ do
      let input = unTestFixture getUserInput readFixture
      shouldBe input "input"

  describe "displayPrompt" $ do
    it "sends prompt to output" $ do
      let (input, output) = evalTestFixture (displayPrompt "prompt") ioFixture
      shouldBe output "prompt"
      shouldBe input "response"

  describe "promptForMove" $ do
    it "sends a prompt for a move and receives move as an integer" $ do
      let fixture = def {
          _writeLine = \line -> tell line
        , _readLine = return "4"
      }
      let (input, output) = evalTestFixture promptForMove fixture
      shouldBe output "Please enter the number of the space where you want to make a move"
      shouldBe input (Just 4)

    it "returns an error if input move cannot be read as an integer" $ do
      let fixture = def {
          _writeLine = \line -> tell line
        , _readLine = return "not an integer"
      }
      let (input, output) = evalTestFixture promptForMove fixture
      shouldBe input Nothing

  describe "displayError" $ do
    it "sends invalid space error message to output" $ do
      let errorMessage = "That space does not exist on the board"
      let output = logTestFixture (displayError InvalidSpaceError) writeFixture
      shouldBe output errorMessage

    it "sends occupied space error message to output" $ do
      let errorMessage = "That space is already occupied"
      let output = logTestFixture (displayError OccupiedSpaceError) writeFixture
      shouldBe output errorMessage

    it "sends not a number error message to output" $ do
      let errorMessage = "That is not a number"
      let output = logTestFixture (displayError NotANumberError) writeFixture
      shouldBe output errorMessage

  describe "displayOutcome" $ do
    it "sends outcome when player one wins to output" $ do
      let outcomeMessage = "Player One wins"
      let output = logTestFixture (displayOutcome PlayerOneWin) writeFixture
      shouldBe output outcomeMessage

    it "sends outcome when player two wins to output" $ do
      let outcomeMessage = "Player Two wins"
      let output = logTestFixture (displayOutcome PlayerTwoWin) writeFixture
      shouldBe output outcomeMessage

    it "sends outcome when game is a draw to output" $ do
      let outcomeMessage = "Game ends in a draw"
      let output = logTestFixture (displayOutcome Draw) writeFixture
      shouldBe output outcomeMessage

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
      let output = logTestFixture (displayGame game) writeFixture
      shouldBe output gameString
