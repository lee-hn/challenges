{-# LANGUAGE GADTs #-}

module Settings
    (
      PlayerData(Players)
    , playerOne
    , playerTwo
    ) where

import UI
    (
      MonadUI
    , promptForPlayerOrder
    , displayError
    )
import Errors ( ErrorData(PlayerOrderError) )
import Human ( HumanPlayerData(HumanPlayer) )
import Computer ( ComputerPlayerData(ComputerPlayer) )

data PlayerData one two = Players {
      playerOne :: one
    , playerTwo :: two
    } deriving (Show, Eq)

