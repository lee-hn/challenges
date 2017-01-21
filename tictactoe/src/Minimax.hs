module Minimax
    (
      scoreMove
    , possibleMoves
    , scorePossibleMoves
    , chooseBestMove
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
import qualified Data.Map as Map

scoreMove :: Int -> GameData -> Int
scoreMove move game
    | isWin updatedGame = 1
    | isDraw updatedGame = 0
    | otherwise = do
        let moveScores = scorePossibleMoves updatedGame
        let bestMove = chooseBestMove moveScores
        negate $ snd bestMove
  where board = boardData game
        moves = movesData game
        updatedMoves = addMove move moves
        updatedGame = Game board updatedMoves

possibleMoves :: GameData -> [Int]
possibleMoves game
    | isWin game = []
    | isDraw game = []
    | otherwise = sort $ spaces \\ moves
  where spaces = boardSpaces $ boardData game
        moves = allMoves $ movesData game

scorePossibleMoves :: GameData -> [(Int, Int)]
scorePossibleMoves game = zip spaces scores
  where spaces = possibleMoves game
        scores = map (\move -> scoreMove move game) spaces

chooseBestMove :: [(Int, Int)] -> (Int, Int)
chooseBestMove moveScores = head bestMoves
  where moveScoresMap = Map.fromList moveScores
        bestScore = maximum $ snd $ unzip moveScores
        bestMoves = Map.toList $ Map.filter (== bestScore) moveScoresMap
