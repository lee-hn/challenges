module Players.Player
    (
      PlayerClass
    , makeMove
    ) where

import Components.Rules ( GameData )
import UI.UI ( MonadUI )

class PlayerClass player where
    makeMove :: MonadUI monad => player -> GameData -> monad Int

