module Human
    (
      HumanPlayerData(HumanPlayer)
    , makeMove
    ) where

import Player
    (
      PlayerClass
    , makeMove
    )
import Rules
    (
      GameData(Game)
    , boardData
    , movesData
    , isSpace
    , isUnoccupied
    )
import UI
    (
      MonadUI
    , promptForMove
    , displayError
    )
import Errors
    (
      ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError)
    )
import Data.Maybe

data HumanPlayerData = HumanPlayer
                     deriving (Show, Eq)

instance PlayerClass HumanPlayerData where
    makeMove human game = do
        move <- promptForMove
        let validation = (
                           isNothing move
                         , isSpace move board
                         , isUnoccupied move moves
                         )
        case validation of
          (True, _, _) -> do
              displayError NotANumberError
              makeMove human game
          (_, False, _) -> do
              displayError InvalidSpaceError
              makeMove human game
          (_, _, False) -> do
              displayError OccupiedSpaceError
              makeMove human game
          (False, True, True) -> do
              return $ fromJust move
      where board = boardData game
            moves = movesData game

