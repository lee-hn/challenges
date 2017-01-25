{-# LANGUAGE GADTs #-}

module Settings
    (
      PlayerData(Players)
    , playerOne
    , playerTwo
    ) where

import Players.Human ( HumanPlayerData(HumanPlayer) )
import Players.Computer ( ComputerPlayerData(ComputerPlayer) )

data PlayerData one two = Players {
      playerOne :: one
    , playerTwo :: two
    } deriving (Show, Eq)

