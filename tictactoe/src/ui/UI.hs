module UI.UI
    (
      MonadUI
    , writeLine
    , readLine
    , displayMessage
    , getUserInput
    , displayPrompt
    , promptForMove
    , promptForPlayerOrder
    , displayError
    , displayOutcome
    , displayGame
    , convertGameToString
    , labelSpace
    , displayTitle
    , displayGameBegin
    , displayGameOver
    , displayGoodbye
    , promptForNewGame
    ) where

import Components.Rules
    (
      GameData
    , boardData
    , movesData
    , OutcomeData(PlayerOneWin, PlayerTwoWin, Draw)
    )
import Components.Moves
    (
      MovesData
    , allMoves
    )
import Components.Board
    (
      BoardData
    , boardDimension
    , boardSpaces
    )
import UI.Errors
    (
      ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError, InvalidResponseError)
    )
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe

class Monad monad => MonadUI monad where
    writeLine :: String -> monad ()
    readLine :: monad String

instance MonadUI IO where
    writeLine = putStrLn
    readLine = getLine

displayMessage :: MonadUI monad => String -> monad ()
displayMessage message = writeLine $ message ++ "\n"

getUserInput :: MonadUI monad => monad String
getUserInput = readLine

displayPrompt :: MonadUI monad => String -> monad String
displayPrompt prompt = do
    displayMessage prompt
    getUserInput

promptForMove :: MonadUI monad => monad (Maybe Int)
promptForMove = do
    move <- (displayPrompt "Please enter the number of the space where you want to make a move")
    if length move > 0 && all isDigit move
        then return $ Just (read move :: Int)
        else return Nothing

promptForPlayerOrder :: MonadUI monad => monad Int
promptForPlayerOrder = do
    order <- (displayPrompt "Do you want to play first or second? Please enter 1 or 2")
    if length order == 1 && isInfixOf order "12"
        then return (read order :: Int)
        else do
            displayError InvalidResponseError
            promptForPlayerOrder

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

displayGame :: MonadUI monad => GameData -> monad ()
displayGame game = displayMessage gameString
  where gameString = convertGameToString game

convertGameToString :: GameData -> String
convertGameToString game = grid
  where board = boardData game
        dim = boardDimension board
        spaces = boardSpaces board
        labels = map (\space -> labelSpace space game) spaces
        rows = map (\row -> intercalate "|" row)
                   (chunksOf dim labels)
        rowSep = intercalate "+" $ replicate dim $ replicate dim '-'
        grid = intercalate ("\n" ++ rowSep ++ "\n") rows

labelSpace :: Int -> GameData -> String
labelSpace space game
    | notElem space moves = " " ++ show space ++ " "
    | even $ moveIndex = " " ++ "X" ++ " "
    | odd $ moveIndex = " " ++ "O" ++ " "
  where moves = allMoves $ movesData game
        moveIndex = fromJust $ elemIndex space moves

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

promptForNewGame :: MonadUI monad => monad String
promptForNewGame = do
    playAgain <- (displayPrompt "Do you want to play again? Please enter Y or N")
    let response = map toUpper playAgain
    if length response == 1 && isInfixOf response "YN"
        then return response
        else do
            displayError InvalidResponseError
            promptForNewGame
