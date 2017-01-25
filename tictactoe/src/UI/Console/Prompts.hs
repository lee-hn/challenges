module UI.Console.Prompts
    (
      promptForMove
    , promptForPlayerOrder
    , promptForNewGame
    ) where

import UI.Console.Base ( displayPrompt )
import UI.Console.Messages ( displayError )
import UI.Errors ( ErrorData(InvalidResponseError) )
import UI.UI ( MonadUI )
import Data.List
import Data.Char

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

promptForNewGame :: MonadUI monad => monad String
promptForNewGame = do
    playAgain <- (displayPrompt "Do you want to play again? Please enter Y or N")
    let response = map toUpper playAgain
    if length response == 1 && isInfixOf response "YN"
        then return response
        else do
            displayError InvalidResponseError
            promptForNewGame
