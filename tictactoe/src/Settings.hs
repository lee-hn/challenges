module Settings
    (
      PlayerData(Players)
    , playerOne
    , playerTwo
    ) where

import Moves ( MovesData(GameState) )

import Player
    ( PlayerClass
    , PlayerTypeData(Computer, Human, Mock)
    )

data PlayerData = Players {
                    playerOne :: PlayerTypeData
                  , playerTwo :: PlayerTypeData
                  } deriving (Show, Eq)
