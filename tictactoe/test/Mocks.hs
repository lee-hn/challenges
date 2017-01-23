module Mocks
    (
      MockPlayerData(MockPlayer)
    ) where

import Players.Player
    (
      PlayerClass
    , makeMove
    )
import UI.UI
    (
      MonadUI
    , writeLine
    , readLine
    )
import Control.Monad.Writer

data MockPlayerData = MockPlayer
                    deriving (Show, Eq)

instance PlayerClass MockPlayerData where
    makeMove mock game = return 0
