module Players.Human
    (
      HumanPlayerData(HumanPlayer)
    , makeMove
    , isValid
    ) where

import Players.Player
    (
      PlayerClass
    , makeMove
    )
import Components.Rules
    (
      GameData
    , boardData
    , movesData
    , isSpace
    , isUnoccupied
    )
import UI.UI ( MonadUI )
import UI.Console.Prompts ( promptForMove )
import UI.Console.Messages ( displayError )
import UI.Errors
    (
      ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError)
    )
import Data.Maybe

data HumanPlayerData = HumanPlayer deriving (Show, Eq)

instance PlayerClass HumanPlayerData where
    makeMove human game = do
        move <- promptForMove
        valid <- isValid move game
        if valid
            then return $ fromJust move
            else makeMove human game

isValid :: MonadUI monad => Maybe Int -> GameData -> monad Bool
isValid move game = do
    let validation = (
                       isNothing move
                     , isSpace move board
                     , isUnoccupied move moves
                     )
    case validation of
      (True, _, _) -> do
         displayError NotANumberError
         return False
      (_, False, _) -> do
         displayError InvalidSpaceError
         return False
      (_, _, False) -> do
         displayError OccupiedSpaceError
         return False
      (False, True, True) -> return True
  where board = boardData game
        moves = movesData game
