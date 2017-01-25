module Main where

import Components.Board ( BoardData(Rows) )
import Components.Moves ( MovesData(GameState) )
import Components.Rules ( GameData(Game) )
import Game ( runGame )
import Settings ( PlayerData(Players) )
import Players.Computer ( ComputerPlayerData(ComputerPlayer) )
import Players.Human ( HumanPlayerData(HumanPlayer) )
import UI.UI
    (
      promptForPlayerOrder
    , displayGame
    , displayTitle
    , displayGameBegin
    , displayGameOver
    , promptForNewGame
    , displayGoodbye
    )

main :: IO ()
main = do
    displayTitle

    let board = Rows 3
    let newGame = GameState []
    let game = Game board newGame
    let computerFirst = Players ComputerPlayer HumanPlayer
    let humanFirst = Players HumanPlayer ComputerPlayer

    order <- promptForPlayerOrder

    displayGameBegin game

    case order of
      1 -> runGame game humanFirst
      2 -> runGame game computerFirst

    displayGameOver

    playAgain <- promptForNewGame

    if playAgain == "Y"
        then main
        else displayGoodbye
