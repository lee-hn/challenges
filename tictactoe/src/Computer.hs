module Computer
    (
      ComputerPlayerData(ComputerPlayer)
    , makeMove
    ) where

import Player
    (
      PlayerClass
    , makeMove
    )

data ComputerPlayerData = ComputerPlayer 
                        deriving (Show, Eq)

instance PlayerClass ComputerPlayerData where
    makeMove computer game = 0
