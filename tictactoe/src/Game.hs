module Game
    (
      runGame
    , endGame
    , nextMove
    ) where

import Rules
    (
      GameData(Game)
    , OutcomeData(Continue)
    , boardData
    , movesData
    , outcome
    , nextPlayer
    )
import Moves
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
import Player
    (
      PlayerClass
    , makeMove
    )
import UI
    (
      MonadUI
    , displayMessage
    , displayOutcome
    , displayGame
    )

runGame :: (MonadUI monad, PlayerClass one, PlayerClass two) => GameData -> (PlayerData one two) -> monad ()
runGame game players
    | outcome game == Continue = do
        newMove <- nextMove game players
        let updatedMoves = addMove newMove moves
        let updatedGame = Game board updatedMoves
        let turnNumber = show $ length $ allMoves updatedMoves
        displayMessage $ "\nTurn " ++ turnNumber ++ ":"
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

