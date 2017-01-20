module Settings
    (
      PlayerData(Players)
    , playerOne
    , playerTwo
    ) where

import Player ( PlayerClass )

data PlayerData one two = Players {
                            playerOne :: one,
                            playerTwo :: two 
                          }
                        deriving (Show, Eq)
