module Player
    (
      PlayerClass
    , PlayerTypeData(Computer, Human, Mock)
    , makeMove
    ) where

import Rules
    (
      GameData(Game)
    , boardData
    , movesData
    , isSpace
    , isUnoccupied
    )

data PlayerTypeData = Computer | Human | Mock
                    deriving (Show, Eq)

class PlayerClass player where
    makeMove :: player -> GameData -> Int

instance PlayerClass PlayerTypeData where
    makeMove Mock game = 0

{- commenting out for now  
    makeMove Computer game = 0
    makeMove Human game
        | not $ isSpace move board = makeMove Human game
        | not $ isUnoccupied move moves = makeMove Human game
        | otherwise = move
      where move = 0
            board = boardData game
            moves = movesData game -}
