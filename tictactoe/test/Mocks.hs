module Mocks
    (
      MockPlayerData(MockPlayer)
    ) where

import Players.Player
    (
      PlayerClass
    , makeMove
    )

data MockPlayerData = MockPlayer
                    deriving (Show, Eq)

instance PlayerClass MockPlayerData where
    makeMove mock game = return 0
