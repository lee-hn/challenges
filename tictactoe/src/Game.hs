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
      PlayerTypeData(Computer, Human, Mock)
    , makeMove
    )

runGame :: GameData -> PlayerData -> String
runGame game players
    | outcome game == Continue = do
        let newMove = nextMove game players
        let updatedMoves = addMove newMove moves
        let updatedGame = Game board updatedMoves
        runGame updatedGame players
    | otherwise = endGame $ outcome game
  where board = boardData game
        moves = movesData game

endGame :: OutcomeData -> String
endGame gameOutcome
    | gameOutcome == PlayerOneWin = "Player One wins"
    | gameOutcome == PlayerTwoWin = "Player Two wins"
    | gameOutcome == Draw = "Draw"

nextMove :: GameData -> PlayerData -> Int
nextMove game players
    | player == 0 = makeMove (playerOne players) game
    | player == 1 = makeMove (playerTwo players) game
  where moves = movesData game
        player = nextPlayer moves