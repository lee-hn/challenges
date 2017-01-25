module UI.Console.Messages
    (
      displayError
    , displayOutcome
    , displayTurn
    , displayGame
    , displayTitle
    , displayGameBegin
    , displayGameOver
    , displayGoodbye
    ) where

import UI.Console.Render
    (
      convertGameToString
    , labelSpace
    )
import UI.Console.Base
    (
      displayMessage
    )
import UI.Errors
    (
      ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError, InvalidResponseError)
    )
import UI.UI( MonadUI )
import Components.Rules
    (
      GameData
    , OutcomeData(PlayerOneWin, PlayerTwoWin, Draw)
    )

displayError :: MonadUI monad => ErrorData -> monad ()
displayError error
    | error == InvalidSpaceError = displayMessage "That space does not exist on the board"
    | error == OccupiedSpaceError = displayMessage "That space is already occupied"
    | error == NotANumberError = displayMessage "That is not a number"
    | error == InvalidResponseError = displayMessage "That is not a valid response"

displayOutcome :: MonadUI monad => OutcomeData -> monad ()
displayOutcome outcome
    | outcome == PlayerOneWin = displayMessage "Player One wins"
    | outcome == PlayerTwoWin = displayMessage "Player Two wins"
    | outcome == Draw = displayMessage "Game ends in a draw"

displayTurn :: MonadUI monad => String -> monad ()
displayTurn turnNumber = displayMessage $ "\nTurn " ++ turnNumber ++ ":"

displayGame :: MonadUI monad => GameData -> monad ()
displayGame game = displayMessage gameString
  where gameString = convertGameToString game

displayTitle :: MonadUI monad => monad ()
displayTitle = displayMessage "\nTIC TAC TOE\n==========="

displayGameBegin :: MonadUI monad => GameData -> monad ()
displayGameBegin game = do
    displayMessage "\nGAME BEGIN"
    displayGame game
    displayMessage "Player 1 = X | Player 2 = O"

displayGameOver :: MonadUI monad => monad ()
displayGameOver = displayMessage "GAME OVER"

displayGoodbye :: MonadUI monad => monad ()
displayGoodbye = displayMessage "\nGoodbye!"
