module Game
    (
      runGame
    , endGame
    , nextMove
    ) where

import Rules
    (
      GameData(Game)
    , OutcomeData(PlayerOneWin, PlayerTwoWin, Draw, Continue)
    , boardData
    , movesData
    , outcome
    , nextPlayer
    )
import Moves
    (
      MovesData(GameState)
    , addMove
    , lastPlayer
    )
import Settings
    (
      PlayerData(Players)
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
    , displayOutcome
    )

runGame :: (PlayerClass one, PlayerClass two, MonadUI monad) => GameData -> (PlayerData one two) -> monad ()
runGame game players
    | outcome game == Continue = do
        newMove <- nextMove game players
        let updatedMoves = addMove newMove moves
        let updatedGame = Game board updatedMoves
        runGame updatedGame players
    | otherwise = endGame $ outcome game
  where board = boardData game
        moves = movesData game

endGame :: MonadUI monad => OutcomeData -> monad ()
endGame gameOutcome = displayOutcome gameOutcome

nextMove :: (PlayerClass one, PlayerClass two, MonadUI monad) => GameData -> (PlayerData one two) -> monad Int
nextMove game players
    | player == 0 = makeMove (playerOne players) game
    | player == 1 = makeMove (playerTwo players) game
  where moves = movesData game
        player = nextPlayer moves
