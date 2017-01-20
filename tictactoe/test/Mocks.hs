module Mocks 
    (
      MockPlayerData(MockPlayer)
    ) where

import Player
    (
      PlayerClass
    , makeMove
    )

data MockPlayerData = MockPlayer
                    deriving (Show, Eq)

instance PlayerClass MockPlayerData where
    makeMove mock game = 0

