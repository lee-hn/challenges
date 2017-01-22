module Mocks
    (
      MockPlayerData(MockPlayer)
    ) where

import Player
    (
      PlayerClass
    , makeMove
    )
import UI
    (
      MonadUI
    , writeLine
    , readLine
    )
import Control.Monad.Writer

data MockPlayerData = MockPlayer
                    deriving (Show, Eq)

instance PlayerClass MockPlayerData where
    makeMove mock game = 0

