module Player
    (
      PlayerClass
    , makeMove
    ) where

import Rules ( GameData )
import UI ( MonadUI )

class PlayerClass player where
    makeMove :: MonadUI monad => player -> GameData -> monad Int

