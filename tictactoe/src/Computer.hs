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
import Minimax
    (
      scorePossibleMoves
    , chooseBestMove
    )

data ComputerPlayerData = ComputerPlayer
                        deriving (Show, Eq)

instance PlayerClass ComputerPlayerData where
    makeMove computer game = fst bestMove
      where moveScores = scorePossibleMoves game
            bestMove = chooseBestMove moveScores
