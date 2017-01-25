{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module UI.Console.MessagesSpec where

import Test.Hspec
import UI.Console.Messages
import UI.UI ( MonadUI )
import UI.Errors ( ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError, InvalidResponseError) )
import Components.Board ( BoardData(Rows) )
import Components.Moves ( MovesData(GameState) )
import Components.Rules
    (
      GameData(Components)
    , OutcomeData(PlayerOneWin, PlayerTwoWin, Draw)
    )
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [''MonadUI]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let newGame = GameState []

  let writeFixture = def {
      _writeLine = \line -> tell line
  }

  describe "displayError" $ do
    it "sends invalid space error message to output" $ do
      let errorMessage = "That space does not exist on the board\n"
      let output = logTestFixture (displayError InvalidSpaceError) writeFixture
      shouldBe output errorMessage

    it "sends occupied space error message to output" $ do
      let errorMessage = "That space is already occupied\n"
      let output = logTestFixture (displayError OccupiedSpaceError) writeFixture
      shouldBe output errorMessage

    it "sends not a number error message to output" $ do
      let errorMessage = "That is not a number\n"
      let output = logTestFixture (displayError NotANumberError) writeFixture
      shouldBe output errorMessage

    it "sends invalid response error message to output" $ do
      let errorMessage = "That is not a valid response\n"
      let output = logTestFixture (displayError InvalidResponseError) writeFixture
      shouldBe output errorMessage

  describe "displayOutcome" $ do
    it "sends outcome when player one wins to output" $ do
      let outcomeMessage = "Player One wins\n"
      let output = logTestFixture (displayOutcome PlayerOneWin) writeFixture
      shouldBe output outcomeMessage

    it "sends outcome when player two wins to output" $ do
      let outcomeMessage = "Player Two wins\n"
      let output = logTestFixture (displayOutcome PlayerTwoWin) writeFixture
      shouldBe output outcomeMessage

    it "sends outcome when game is a draw to output" $ do
      let outcomeMessage = "Game ends in a draw\n"
      let output = logTestFixture (displayOutcome Draw) writeFixture
      shouldBe output outcomeMessage

  describe "displayTurn" $ do
    it "displays the turn number" $ do
      let turnNumber = "1"
      let output = logTestFixture (displayTurn turnNumber) writeFixture
      shouldBe output "\nTurn 1:\n"

  describe "displayGame" $ do
    it "displays string representation of a game" $ do
      let gameString = " 0 | 1 | 2 \n\
                       \---+---+---\n\
                       \ 3 | 4 | 5 \n\
                       \---+---+---\n\
                       \ 6 | 7 | 8 "
      let game = Components board newGame
      let output = logTestFixture (displayGame game) writeFixture
      shouldBe output (gameString ++ "\n")

  describe "displayTitle" $ do
    it "displays the game title" $ do
      let title = "\nTIC TAC TOE\n==========="
      let output = logTestFixture displayTitle writeFixture
      shouldBe output (title ++ "\n")

  describe "displayGameBegin" $ do
    it "displays the game begin message and empty board" $ do
      let game = Components board newGame
      let gameBegin = "\nGAME BEGIN"
      let gameString = " 0 | 1 | 2 \n\
                       \---+---+---\n\
                       \ 3 | 4 | 5 \n\
                       \---+---+---\n\
                       \ 6 | 7 | 8 "
      let playerMarkers = "Player 1 = X | Player 2 = O"
      let output = logTestFixture (displayGameBegin game) writeFixture
      shouldBe output $ gameBegin ++ "\n"
                     ++ gameString ++ "\n"
                     ++ playerMarkers ++ "\n"

  describe "displayGameOver" $ do
    it "displays the game over message" $ do
      let message = "GAME OVER"
      let output = logTestFixture displayGameOver writeFixture
      shouldBe output (message ++ "\n")

  describe "displayGoodbye" $ do
    it "displays the goodbye message" $ do
      let message = "\nGoodbye!"
      let output = logTestFixture displayGoodbye writeFixture
      shouldBe output (message ++ "\n")

