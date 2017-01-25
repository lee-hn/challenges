module UI.Console.Render
    (
      convertGameToString
    , labelSpace
    ) where

import Components.Rules
    (
      GameData
    , boardData
    , movesData
    )
import Components.Board
    (
      BoardData
    , boardDimension
    , boardSpaces
    )
import Components.Moves
    (
      MovesData
    , allMoves
    )
import Data.List
import Data.List.Split
import Data.Maybe

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

