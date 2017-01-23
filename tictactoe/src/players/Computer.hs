module Players.Computer
    (
      ComputerPlayerData(ComputerPlayer)
    , makeMove
    ) where

import Players.Player
    (
      PlayerClass
    , makeMove
    )
import Components.Minimax
    (
      scorePossibleMoves
    , chooseBestMove
    )

data ComputerPlayerData = ComputerPlayer deriving (Show, Eq)

instance PlayerClass ComputerPlayerData where
    makeMove computer game = do
        return $ fst bestMove
      where moveScores = scorePossibleMoves game
            bestMove = chooseBestMove moveScores
