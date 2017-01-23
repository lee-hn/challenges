module Main where

import Lib
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )
import Rules ( GameData(Game) )
import Game ( runGame )
import Settings ( PlayerData(Players) )
import Computer ( ComputerPlayerData(ComputerPlayer) )
import Human ( HumanPlayerData(HumanPlayer) )
import UI
    (
      displayMessage
    , promptForPlayerOrder
    , displayGame
    )

main :: IO ()
main = do
    let board = Rows 3
    let newGame = GameState []
    let game = Game board newGame
    let computerFirst = Players ComputerPlayer HumanPlayer
    let humanFirst = Players HumanPlayer ComputerPlayer

    displayMessage "\nTIC TAC TOE\n============"

    order <- promptForPlayerOrder

    displayMessage "\nSTART GAME"
    displayGame game

    case order of
      1 -> runGame game humanFirst
      2 -> runGame game computerFirst

    displayMessage "GAME OVER"
