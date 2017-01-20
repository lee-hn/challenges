module Human 
    (
      HumanPlayerData(HumanPlayer)
    ) where

import Player
    (
      PlayerClass
    , makeMove
    )

data HumanPlayerData = HumanPlayer
                     deriving (Show, Eq)

instance PlayerClass HumanPlayerData where
    makeMove human game = 0

{- commenting out for now  
    makeMove Human game
        | not $ isSpace move board = makeMove Human game
        | not $ isUnoccupied move moves = makeMove Human game
        | otherwise = move
      where move = 0
            board = boardData game
            moves = movesData game -}


