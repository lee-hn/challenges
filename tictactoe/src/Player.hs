module Player
    (
      PlayerClass
    , makeMove
    ) where

import Rules ( GameData(Game) )

class PlayerClass player where
    makeMove :: player -> GameData -> Int

