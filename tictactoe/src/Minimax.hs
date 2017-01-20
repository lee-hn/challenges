module Minimax
    (
      scoreMove
    , possibleMoves
    , scorePossibleMoves
    ) where

import Rules
    (
      GameData(Game)
    , boardData
    , movesData
    , isWin
    , isDraw
    , outcome
    , nextPlayer
    )
import Moves
    (
      MovesData(GameState)
    , allMoves
    , addMove
    , lastPlayer
    )
import Board
    (
      BoardData(Rows)
    , boardSpaces
    )
import Data.List

scoreMove :: Int -> GameData -> Int
scoreMove move game
    | isWin updatedGame = 1
    | isDraw updatedGame = 0
    | otherwise = negate $ maximum $ scorePossibleMoves updatedGame
  where board = boardData game 
        moves = movesData game 
        updatedMoves = addMove move moves
        updatedGame = Game board updatedMoves

possibleMoves :: GameData -> [Int]
possibleMoves game
    | isWin game = []
    | isDraw game = []
    | otherwise = spaces \\ moves
  where spaces = boardSpaces $ boardData game
        moves = allMoves $ movesData game

scorePossibleMoves :: GameData -> [Int]
scorePossibleMoves game = map (\move -> scoreMove move game) 
                              (possibleMoves game)

