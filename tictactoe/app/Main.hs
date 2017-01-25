module Main where

import Components.Board ( BoardData(Rows) )
import Components.Moves ( MovesData(GameState) )
import Components.Rules ( GameData(Components) )
import Game ( runGame )
import Settings ( PlayerData(Players) )
import Players.Computer ( ComputerPlayerData(ComputerPlayer) )
import Players.Human ( HumanPlayerData(HumanPlayer) )
import UI.Console.Prompts
    (
      promptForPlayerOrder
    , promptForNewGame
    )
import UI.Console.Messages
    (
      displayGame
    , displayTitle
    , displayGameBegin
    , displayGameOver
    , displayGoodbye
    )

main :: IO ()
main = do
    displayTitle

    let board = Rows 3
    let newGame = GameState []
    let game = Components board newGame
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
