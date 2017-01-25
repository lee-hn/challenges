module Game
    (
      runGame
    , endGame
    , nextMove
    ) where

import Components.Rules
    (
      GameData(Components)
    , boardData
    , movesData
    , OutcomeData(Continue)
    , outcome
    , nextPlayer
    )
import Components.Moves
    (
      MovesData
    , allMoves
    , addMove
    , lastPlayer
    )
import Settings
    (
      PlayerData
    , playerOne
    , playerTwo
    )
import Players.Player
    (
      PlayerClass
    , makeMove
    )
import UI.UI ( MonadUI )
import UI.Console.Messages (
      displayOutcome
    , displayTurn
    , displayGame
    )

runGame :: (MonadUI monad, PlayerClass one, PlayerClass two) => GameData -> (PlayerData one two) -> monad ()
runGame game players
    | outcome game == Continue = do
        newMove <- nextMove game players
        let updatedMoves = addMove newMove moves
        let updatedGame = Components board updatedMoves
        let turnNumber = show $ length $ allMoves updatedMoves
        displayTurn turnNumber
        displayGame updatedGame
        runGame updatedGame players
    | otherwise = endGame $ outcome game
  where board = boardData game
        moves = movesData game

endGame :: MonadUI monad => OutcomeData -> monad ()
endGame gameOutcome = displayOutcome gameOutcome

nextMove :: (MonadUI monad, PlayerClass one, PlayerClass two) => GameData -> (PlayerData one two) -> monad Int
nextMove game players
    | player == 0 = makeMove (playerOne players) game
    | player == 1 = makeMove (playerTwo players) game
  where moves = movesData game
        player = nextPlayer moves

