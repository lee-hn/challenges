module UI
    (
      MonadUI
    , writeLine
    , readLine
    , displayMessage
    , getUserInput
    , displayPrompt
    , displayGame
    , convertGameToString
    , labelSpace
    ) where

import Rules
    (
      GameData(Game)
    , boardData
    , movesData
    )
import Moves
    (
      MovesData(GameState)
    , allMoves
    )
import Board
    (
      BoardData(Rows)
    , boardDimension
    , boardSpaces
    )
import Data.List
import Data.List.Split
import Data.Maybe

class Monad monad => MonadUI monad where
    writeLine :: String -> monad ()
    readLine :: monad String

instance MonadUI IO where
    writeLine = putStrLn
    readLine = getLine

displayMessage :: MonadUI monad => String -> monad ()
displayMessage message = writeLine message

getUserInput :: MonadUI monad => monad String
getUserInput = readLine

displayPrompt :: MonadUI monad => String -> monad String
displayPrompt prompt = do
    writeLine prompt
    readLine

displayGame :: MonadUI monad => GameData -> monad ()
displayGame game = displayMessage gameString
  where gameString = convertGameToString game

convertGameToString :: GameData -> String
convertGameToString game = grid
  where board = boardData game
        dim = boardDimension board
        spaces = boardSpaces board
        labels = map (\space -> labelSpace space game) spaces
        rows = map (\row -> intercalate "|" row)
                   (chunksOf dim labels)
        rowSep = intercalate "+" $ replicate dim $ replicate dim '-'
        grid = intercalate ("\n" ++ rowSep ++ "\n") rows

labelSpace :: Int -> GameData -> String
labelSpace space game
    | notElem space moves = " " ++ show space ++ " "
    | even $ moveIndex = " " ++ "X" ++ " "
    | odd $ moveIndex = " " ++ "O" ++ " "
  where moves = allMoves $ movesData game
        moveIndex = fromJust $ elemIndex space moves
